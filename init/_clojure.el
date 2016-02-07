;; -*-emacs-lisp-*-


;;;
;;; Clojure configuration
;;;

(eval-when-compile
  (require 'cider))

(defun run-clojure ()
  (interactive)
  (if (or (not (fboundp 'cider-connected-p))
          (not (cider-connected-p)))
      (call-interactively 'cider-jack-in)
    (cider-switch-to-repl-buffer)))

(eval-after-load "clojure-mode"
  '(when (boundp 'clojure-mode-map)
     (define-key clojure-mode-map [(control ?c) ?\!]
       'run-clojure)))
