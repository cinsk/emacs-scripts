;; -*-emacs-lisp-*-


;;;
;;; Clojure configuration
;;;

(defun run-clojure ()
  (interactive)
  (if (or (not (fboundp 'cider-connected-p))
          (not (cider-connected-p)))
      (call-interactively 'cider-jack-in)
    (cider-switch-to-repl-buffer)))

(with-eval-after-load "clojure-mode"
  (when (locate-library "paredit")
    (add-hook 'clojure-mode-hook 'paredit-mode))
  (when (boundp 'clojure-mode-map)
    (define-key clojure-mode-map [(control ?c) ?\!] 'run-clojure)))

(with-eval-after-load "company-mode"
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode))
