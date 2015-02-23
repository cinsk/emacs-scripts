;; -*-emacs-lisp-*-

;;;
;;; ESS configuration
;;;

(when (locate-library "ess-site")
  (require 'ess-site))

(defun ess-shell (eob-p)
  "Switch to the current inferior ESS process buffer.

If there is no ESS buffer, create one.  See `ess-switch-to-ESS' for EOB-P."
  (interactive "P")
  (ess-force-buffer-current "Process to load into: ")
  (ess-switch-to-ESS eob-p))

(eval-after-load "ess-mode"
  '(progn
     (when (boundp 'ess-mode-map)
       (define-key ess-mode-map [(control ?c) ?\!] 'ess-shell)
       (define-key ess-mode-map [(control ?c) (control ?q)] 'ess-dev-off)
       (define-key ess-mode-map [(control ?c) ?q] 'ess-quit))
     (when (boundp 'inferior-ess-mode-map)
       (define-key ess-mode-map [(control ?c) (control ?q)] 'ess-dev-off)
       (define-key ess-mode-map [(control ?c) ?q] 'ess-quit))))



(defun R-table-region (beg end)
  (interactive "r")
  (let ((buf (current-buffer))
        (fifo (make-temp-file "fifo"))
        (tmpfile (make-temp-file "ess")))
    (with-temp-file tmpfile
      (insert-buffer-substring-no-properties buf beg end)
      ;; R complains if the data file does not end with newline
      (let ((lchar (char-after (1- (point-max)))))
        (when (and (/= lchar ?\n) (/= lchar ?\r))
          (goto-char (point-max))
          (insert ?\n))))
    (shell-command-to-string
     (format "rm -f %s && mkfifo %s" fifo fifo))
    (ess-display-help-on-object "read.table")
    (let ((params (read-from-minibuffer
                   (format "additional parameter(s) [%s]: " tmpfile))))
      (let ((proc (start-process-shell-command
                   "ess-fifo" nil
                   (format "cat %s > %s" tmpfile fifo))))
        (process-put proc 'fifo fifo)
        (process-put proc 'data tmpfile)
        (set-process-sentinel
         proc
         (lambda (p event)
           ;; (message "sentinel[%S] %S %s" p (process-status p) event)
           (ignore-errors (delete-file (process-get p 'fifo)))
           (ignore-errors (delete-file (process-get p 'data))))))
      (ess-switch-to-ESS 'eob-p)
      (ess-eval-linewise
       (format "read.table(fifo(\"%s\"), %s)" fifo params))
                                        ;(ess-switch-to-ESS 'eob-p)
      (message "You can access the table from \".Last.value\"")))
  ;;(ignore-errors (delete-file fifo))
  ;;(ignore-errors (delete-file tmpfile))
  )


(defvar ess-number-regexp
  "\\`[+-]?\\(0?[0-9.]+\\|0x[0-9.a-fA-F]+\\)\\([eE][+-][0-9]+\\)?\\'"
  "The regular expression that matches a R(ESS) numerical constant")

(defun ess-eval-string (str)
  (with-temp-buffer
    (insert str)
    (ess-eval-buffer nil)))

(defun ess-dev-off ()
  (interactive)
  (ess-eval-string "dev.off()"))

(defun ess-parse-as-vector (start end)
  "Parse the region and evaluate it as a vector in the inferior R(ESS) process"
  ;; TODO: perhaps it's better to use scan() function in R
  (interactive "r")
  (let ((srcbuf (current-buffer))
        (tmpbuf (generate-new-buffer " *TMP.R*")))
    (unwind-protect
        (save-excursion
          (save-restriction
            (narrow-to-region start end)
            (with-current-buffer tmpbuf
              (erase-buffer)
              (insert "c("))
            (goto-char (point-min))
            (when (re-search-forward "[^[:space:],\n\r|]+" nil t)
              (let ((matched (match-string-no-properties 0)))
                (with-current-buffer tmpbuf
                  (if (string-match ess-number-regexp matched)
                      (insert matched)
                    (insert (concat "\"" matched "\""))))))
            (while (re-search-forward "[^[:space:],\n\r|]+" nil t)
              (let ((matched (match-string-no-properties 0)))
                (with-current-buffer tmpbuf
                  (if (string-match ess-number-regexp matched)
                      (insert (concat ", " matched))
                    (insert (concat ", \"" matched "\""))))))
            (with-current-buffer tmpbuf
              (insert ")")))
          (with-current-buffer tmpbuf
            (ess-eval-line nil)))
      (kill-buffer tmpbuf))))
