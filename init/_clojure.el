;; -*-emacs-lisp-*-


;;;
;;; Clojure configuration
;;;


(when (boundp 'cider-mode-map)
  (define-key cider-mode-map [(control ?c) ?\!] 'cider-switch-to-repl-buffer))
