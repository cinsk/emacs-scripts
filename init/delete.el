;; -*-emacs-lisp-*-

;;;
;;; text deleting-related configuration
;;;

;;
;; hungray Delete
;;
(defun zap-to-nonspace ()
  "Delete all whitespace up to the next non-whitespace char."
  (interactive)
  (save-excursion
    (let ((start (point))
          (end (point-max)))
      (if (re-search-forward "[^ \n\t\v]" nil t)
          (setq end (min (1- (point)) end)))
      (kill-region start end))))

;; If you just want to delete whitespaces, it may be better to call
;; `just-once-space', which is normally bound to `M-<SPC>'.  This does
;; not work with multiple lines, though.

(defun delete-chars-forward-with-syntax ()
  "Delete forward all characters that have the same syntax element."
  (interactive)
  (let ((beg (point-marker))
        (chr (char-after)))
    (if (not (null chr))
        (progn
          (skip-syntax-forward (string (char-syntax (char-after))))
          (if (not (= beg (point)))
              (kill-region beg (point)))))))

(defun delete-chars-backward-with-syntax ()
  "Delete backward all characters that have the same syntax element.

NOTE: not fully implemented yet."
  (interactive)
  (let ((beg (point-marker))
        (chr (char-after)))
    (if (not (null chr))
        (progn
          (skip-syntax-backward (string (char-syntax chr)))
          ;;(message (buffer-substring-no-properties beg (point)))))))
          (message "%s %s" beg (point))
          (if (not (= beg (point)))
              (kill-region (+ beg 1) (point))
            (delete-char 1))
          (goto-char (- beg 1))))))

(global-set-key [(control ?c) (control ?d)] 'delete-chars-forward-with-syntax)
