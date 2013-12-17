;; -*-emacs-lisp-*-


;;;
;;; Clojure configuration
;;;

(eval-when-compile
  (require 'inf-lisp)
  (require 'comint))

;; inf-lisp.el
(defun clojure-eval-defun (&optional and-go)
  (interactive "P")
  (lisp-eval-defun t))

(defun clojure-eval-last-sexp (&optional and-go)
  (interactive "P")
  (let ((buf (current-buffer)))
    (lisp-eval-last-sexp t)
    (pop-to-buffer buf)))

(defun inferior-clojure (cmd)
  "Run an inferior Clojure process, input and output via buffer `*inferior-lisp*'.
If there is a process already running in `*inferior-lisp*', just
switch to that buffer.  With argument, allows you to edit the
command line (default is value of `inferior-lisp-program').  Runs
the hooks from `inferior-lisp-mode-hook' (after the
`comint-mode-hook' is run).  \(Type \\[describe-mode] in the
process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
                         (read-string "Run clojure: " inferior-lisp-program)
                       inferior-lisp-program)))
  (if (not (comint-check-proc "*inferior-lisp*"))
      (let ((cmdlist (split-string cmd)))
        (set-buffer (apply (function make-comint)
                           "inferior-lisp" (car cmdlist) nil (cdr cmdlist)))
        (inferior-lisp-mode)))
  (setq inferior-lisp-buffer "*inferior-lisp*")
  (pop-to-buffer "*inferior-lisp*"))

(defalias 'run-clojure 'inferior-clojure)

(eval-after-load "clojure-mode"
  '(progn
     (define-key clojure-mode-map [(control ?c) ?\!] 'run-clojure)
     (define-key clojure-mode-map [(control ?c) (control ?e)] 'clojure-eval-last-sexp)
     )
  )
