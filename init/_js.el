;; -*-emacs-lisp-*-

;;;
;;; javascript configuration
;;;

(eval-when-compile
  (require 'js-comint nil t))

(when (locate-library "js-comint")
  (require 'js-comint))

(let ((path (executable-find "js")))
  (when path
    (setq inferior-js-program-command path)))

(eval-after-load "js"
  '(progn
     (define-key js-mode-map [(control ?c) ?\!] 'run-js)
     (define-key js-mode-map [(control ?c) (control ?r)] 'js-send-region-and-go)
     (define-key js-mode-map [(control ?c) (control ?b)] 'js-send-buffer-and-go)))
