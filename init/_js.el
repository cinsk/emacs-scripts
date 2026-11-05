;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;
;;; javascript configuration
;;;

(eval-when-compile
  (if (locate-library "js2-mode")
      (require 'js2-mode)
    (require 'js)
    (require 'js-comint nil t)))


(with-eval-after-load "indium-interaction"
  (my/move-key :keymap indium-interaction-mode-map :from "C-c d" :to "C-c D")
  (keymap-set indium-interaction-mode-map "C-c C-r" #'indium-eval-region)
  (keymap-set indium-interaction-mode-map "C-c C-b" #'indium-eval-buffer)
  (keymap-set indium-interaction-mode-map "C-c !" #'indium-run-node)
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
    (keymap-set js-mode-map "C-c !" #'run-js)
    (keymap-set js-mode-map "C-c C-r" #'js-send-region-and-go)
    (keymap-set js-mode-map "C-c C-b" #'js-send-buffer-and-go)))


(enable-minor-mode skewer-mode
  :major-mode js2-mode
  :when (and buffer-file-name
             (string-prefix-p (expand-file-name "~/src/js/d3/") buffer-file-name)))


(with-eval-after-load "skewer-mode"
  (keymap-set skewer-mode-map "C-c C-b" #'skewer-load-buffer)
  (keymap-set skewer-mode-map "C-c !" #'skewer-repl))
