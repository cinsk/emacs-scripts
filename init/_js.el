;; -*-emacs-lisp-*-

;;;
;;; javascript configuration
;;;

(eval-when-compile
  (if (locate-library "js2-mode")
      (require 'js2-mode)
    (require 'js)
    (require 'js-comint nil t)))


(with-eval-after-load "indium-interaction"
  (cinsk/move-key indium-interaction-mode-map [(control ?c) ?d] [(control ?c) ?D])
  (define-key indium-interaction-mode-map [(control ?c) (control ?r)] 'indium-eval-region)
  (define-key indium-interaction-mode-map [(control ?c) (control ?b)] 'indium-eval-buffer)
  (define-key indium-interaction-mode-map [(control ?c) ?\!] 'indium-run-node)
  )

(if (locate-library "js2-mode")
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
      (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
      (add-to-list 'interpreter-mode-alist '("nodejs" . js2-mode))

      (with-eval-after-load "js2-mode"
        (add-to-list 'js2-mode-hook 'subword-mode)

        (when (locate-library "indium")
          (add-hook 'js2-mode-hook (lambda ()
                                     (unless (fboundp 'indium-interaction-mode)
                                       (require 'indium))
                                     (indium-interaction-mode))))))
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


(enable-minor-mode "js2-mode" js2-mode-hook skewer-mode skewer-mode
  (string-prefix-p (expand-file-name "~/src/js/d3/") buffer-file-name))


(with-eval-after-load "skewer-mode"
  (define-key skewer-mode-map [(control ?c) (control ?b)] 'skewer-load-buffer)
  (define-key skewer-mode-map [(control ?c) ?\!] 'skewer-repl))
