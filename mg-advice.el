;; Asynchronous code talking to the megalodon hammer
;;
;;
;; SYNOPSIS: M-x mg-mode
;; And then typing "aby." inside an mg proof calls the hammer.
;;
;; You should likely customize these variables: mg-binary , mg-hammer-binary .
;;
;; You can also run mg-run-mg or C-cm to run Megalodon on the current buffer.

(require 'cl)
(require 'url)


(defgroup mg-mode nil
  "Major mode for authoring Megalodon articles"
  :group 'languages)

(defcustom mg-binary "/home/user/bin/megalodon"
"Megalodon binary"
:type 'string
:group 'mg-mode)



(defcustom mg-hammer-binary "/home/user/bin/vampire_rel_static_forIsabelle_6878" 
"Binary for the ATP"
:type 'string
:group 'mg-mode)

(defcustom mg-hammer-binary-params
  '("--input_syntax" "tptp" "--proof" "tptp" "--output_axiom_names" "on" "--mode" "portfolio" "--schedule" "snake_tptp_hol" "-t")
"List of default params for the hammer binary"
:type 'list
:group 'mg-mode)

(defcustom mg-hammer-timelimit 10
"Maximum time for which we let one query be asked to the hammer."
:type 'integer
:group 'mg-mode)

(defcustom mg-atp-completion t
"*Dot following \"aby\" calls ATP to provide justification."
:type 'boolean
:group 'mg-mode)


(defun mg-dot (&optional arg)
  "Call ATP on aby."
  (interactive "*p")
  (self-insert-command (prefix-numeric-value arg))
  (if (and mg-atp-completion
	   (looking-back "aby." (- (point) 4)))
;;      (mg-atp-autocomplete)))
      (mg-atp-autocomplete)))

(defun mg-atp-autocomplete ()
"Replace \"aby.\" with \"aby. (** ATP asked ... **)\" and call ATP to justify the current position.
Used automatically if `mg-atp-completion' is on."
(save-excursion
  (let* ((pos (point)) (pos1 (- pos 4)))
    (forward-char -4) 
    (if (looking-at "aby.")
	(progn 
	  (replace-match "aby. (** ATP asked ... **)")
	  ;; We leave one space in the beg and end without the added properties
	  ;; not to get sticky behavior for unsuspecting users.
	  (mg-mark-call-atp pos1 (- (point) 1)))))))

(defvar mg-mode-map (make-sparse-keymap) "Keymap used by mg mode..")

;(define-key mg-mode-map "\C-ca" 'mg-run-hammer)
(define-key mg-mode-map "." 'mg-dot)
(define-key mg-mode-map "\C-cm" 'mg-run-mg)

(defcustom mg-atp-desync-limit 1000
"*Character extent where we try to synchronize ATP output with original text."
:type 'integer
:group 'mg-mode)

(defun mg-take-last-line (output)
  "Return the last line from OUTPUT string."
  (car (last (split-string output "\n" t))))

(defun detptpize (x)
  (let ((r "")
        (i 0)
        (n (length x)))
    (while (< i n)
      (let ((c (aref x i)))
        (if (eq c ?_)
            (if (> (+ i 3) n)
                (progn
		  (setq i n)) ; terminate loop
              (setq r (concat r (string (string-to-number (substring x (1+ i) (+ i 3)) 16))))
              (setq i (+ i 3)))
          (setq r (concat r (string c)))
          (setq i (1+ i)))))
    r))

(defun mg-remove-c-prefix-and-delete-def-suffix (str-list)
  "Remove prefix 'c_' from each string in STR-LIST and completely delete strings ending with '_def'."
  (let (result)
    (dolist (s str-list (nreverse result))
      (unless (string-prefix-p "conj_" s)
      (unless (string-suffix-p "_def" s)
        (push
	 (if (string-prefix-p "c_" s)
                  (detptpize (substring s 2))
                (detptpize s))
              result))))))

(defun mg-grep-buffer-file ()
  "Search the current buffer for all occurrences of file(foo,blah) and return all blah as a comma-separated string."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (matches)
      (while (re-search-forward "file([^,]*,\\([^)]*\\))" nil t)
        (push (match-string 1) matches))
      (mapconcat 'identity (mapcar '(lambda (x) (concat " " x))  (mg-remove-c-prefix-and-delete-def-suffix matches)) "")))) 


(defun mg-insert-atp-result (buf mgpos atpres)
"Try to find in BUF text with property 'atp-asked set to MGPOS and replace with ATPRES."
(save-excursion
  (set-buffer buf)
  (message (buffer-name buf))
  (let* ((mgpoint (string-to-number mgpos))
	 (start (max (point-min) (- mgpoint mg-atp-desync-limit)))
	 (end (min (point-max) (+ mgpoint (* 4 mg-atp-desync-limit))))
	 (pos1 (text-property-any start end 'atp-asked (intern mgpos))))
    (if (not pos1) (message "Position for ATP solution of %s not found" mgpos)
      (save-excursion
	(goto-char pos1)
	(if (not (looking-at "aby. ([*][*] ATP asked ... [*][*])"))
	    (message "Position for ATP solution of %s user-edited. No inserting." mgpos)
	  (replace-match atpres)))))))

(defvar mg-hammer-buffer-data (make-hash-table :test 'eq)
  "Hash table with hammering data stored for each ATP call." )

(defun mg-hammer-sentinel (proc event)
  "Sentinel for `mg-run-hammer'. When PROC (the ATP) exits, it
looks up the results and puts them into the original buffer."
  (when  (memq (process-status proc) '(closed exit signal))
    (let* ((atp-buf (process-buffer proc))
	   (atp-data (gethash atp-buf mg-hammer-buffer-data))
	   (orig-buf (car atp-data))
	   (orig-pos (cadr atp-data)))
      (with-current-buffer atp-buf
	(let* ((res (mg-take-last-line (buffer-string)))
	       (output-str
		(if (string-match "Success" res)
		    (concat "aby" (mg-grep-buffer-file) "." )
		  "aby. (** No ATP proof found **)")
		))
	  
	  (mg-insert-atp-result orig-buf orig-pos output-str)
	  (message "%s" res)
;	  (message "%s" orig-pos)
	  )
	))))

;; Here is how we call megalodon to create the atp problem
;; megalodon -th0single myprob4 8707 100 100thms_12_h.mg

(defun mg-mark-call-atp (beg end)
"Mark the region with the 'help-echo and 'atp-asked property and call ATP with the END position."
(save-excursion
  (goto-char beg)
  (let* ((mod (buffer-modified-p))
	 (pos (number-to-string end))
	 (line   (line-number-at-pos))
	 (col (current-column))
	 (mg-pos (concat (number-to-string line) "_" (number-to-string col)))
	 (buf (buffer-name))
	 (prob-name (concat "mgprob_" mg-pos))
	 (prob-file-name (concat prob-name ".th0.p"))
	 (msg (concat "ATP was called on this step, awaiting response for position " mg-pos)))
    (save-buffer)
    (call-process mg-binary nil nil nil "-th0single" prob-name (number-to-string line) (number-to-string col) (buffer-file-name))
    (put-text-property beg end 'help-echo msg)
    (put-text-property beg end 'atp-asked (intern pos))
;    megalodon -th0single myprob4 8707 100 100thms_12_h.mg
    (mg-run-hammer (current-buffer) end prob-file-name)
    (message "Calling ATP on position %s " mg-pos)
    (set-buffer-modified-p mod))))

(defun mg-run-mg ()
  "Save the current buffer and call the binary 'foo' on its file."
  (interactive)
  (when (buffer-file-name)
    (save-buffer)
    (shell-command (concat mg-binary " " (shell-quote-argument (buffer-file-name))))))

(defun mg-run-hammer (buf insert-pos problem-path)
  "Run `mg-hammer-binary' asynchronously with
`mg-hammer-binary-params', `mg-hammer-timelimit' and
`problem-path'.  After finishing, apply `mg-hammer-sentinel' to
the output and insert the result (a string) into BUF at
INSERT-POS."
  (let* ((output-name (concat "*atp-output-" (buffer-name buf)
			      (int-to-string insert-pos) "*"))
	(output-buffer (generate-new-buffer output-name))
	(data (list buf (int-to-string insert-pos))))
    (puthash output-buffer data mg-hammer-buffer-data)
    (set-process-sentinel
     (apply #'start-process "mghammer" output-buffer mg-hammer-binary
	    (append mg-hammer-binary-params
		    (list (int-to-string mg-hammer-timelimit))
		    (list problem-path)))
     #'mg-hammer-sentinel)))

(defcustom mg-mode-hook nil
  "A hook for mg mode."
  :type 'hook
  :group 'mg-mode)

(defun mg-mode (&optional arg)
  "Major mode for editing Mg articles and viewing Mg abstracts.

Commands:
\\{mg-mode-map}
Entry to this mode calls the value of `mg-mode-hook'
if that value is non-nil."
  (interactive "P")
  (kill-all-local-variables)
  (use-local-map mg-mode-map)
  (setq major-mode 'mg-mode)
  (setq mode-name "Mg")
;  (setq local-abbrev-table mg-mode-abbrev-table)
;  (mg-mode-variables)
  (setq buffer-offer-save t)
;  (mg-setup-imenu-sb)
;  (if (buffer-abstract-p (current-buffer))
;      (mg-set-item-overlays-in-abstract))
;  (if (and mg-abstracts-use-view
;	   (buffer-abstract-p (current-buffer)))
;      (view-mode))
;  (add-to-list 'fontification-functions 'mg-underline-cexpls)
;  (make-local-variable 'font-lock-fontify-region-function)
  (run-hooks 'mg-mode-hook)
  )



(provide 'mg-mode)
