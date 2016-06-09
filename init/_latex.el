;; -*-emacs-lisp-*-

;;;
;;; TeX & LaTeX configuration
;;;

(eval-when-compile
  (require 'tex-mode))

(defun bounds-of-word-markers (&optional no-region)
  "Return the start and end buffer locations for the word at point.

The value is a cons cell (START-MARK . END-MARK) giving the start
and end markers.  If NO-REGION is not nil and there is no word at point,
this function returns a cons cell of current region."
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (if (and (not bounds) (not no-region) mark-active)
        (setq bounds (cons (region-beginning) (region-end))))

    (if bounds
        (cons (set-marker (make-marker) (car bounds))
              (set-marker (make-marker) (cdr bounds))))))

(defun latex-enclose-word (&optional arg)
  "Enclose current word with the supplied command name

After enclosing the current word, this function set the marker at
the beginning of the word, and move the point to the end of the
word.

If a prefix argument is given, this function uses the region
instead of the current word."
  (interactive "P")
  (let ((range (bounds-of-word-markers))
        (collect nil) (default nil))
    (if (boundp 'latex-command-name-history)
        (progn
          (setq collect latex-command-name-history)
          (setq default (car latex-command-name-history))))
    (if range
        (let ((cmdname (completing-read
                        (if default
                            (format "Command name[%s]: " default)
                          "Command name: ")
                        collect nil nil nil
                        'latex-command-name-history default)))
          (goto-char (car range))
          (insert-before-markers (format "\\%s{" cmdname))
          (goto-char (cdr range))
          (insert "}")
          (goto-char (car range))
          (push-mark)
          (goto-char (cdr range))))))

(with-eval-after-load "tex-mode"
  (define-key tex-mode-map [(control ?c) ?e] 'latex-enclose-word))
