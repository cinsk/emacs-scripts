;; -*-emacs-lisp-*-

;;;
;;; javascript configuration
;;;

(eval-when-compile
  (if (locate-library "js2-mode")
      (require 'js2-mode)
    (require 'js)
    (require 'js-comint nil t)))


(if (locate-library "js2-mode")
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
      (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
      (add-to-list 'interpreter-mode-alist '("nodejs" . js2-mode))

      (when (locate-library "indium")
        (add-hook 'js2-mode-hook (lambda ()
                                   (unless (fboundp 'indium-interaction-mode)
                                     (require 'indium))
                                   (indium-interaction-mode)))))
  ;; else part
  (when (locate-library "js-comint")
    (require 'js-comint))

  (let ((path (executable-find "js")))
    (when path
      (setq inferior-js-program-command path)))

  (with-eval-after-load "js"
    (define-key js-mode-map [(control ?c) ?\!] 'run-js)
    (define-key js-mode-map [(control ?c) (control ?r)] 'js-send-region-and-go)
    (define-key js-mode-map [(control ?c) (control ?b)] 'js-send-buffer-and-go)))

(with-eval-after-load "skewer-mode"
  (define-key skewer-mode-map [(control ?c) (control ?b)] 'skewer-load-buffer)
  (define-key skewer-mode-map [(control ?c) ?\!] 'skewer-repl))
