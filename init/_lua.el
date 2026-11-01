;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;
;;; lua configuration
;;;


(with-eval-after-load "lua-mode"
  ;;(keymap-set lua-mode-map "C-c !" #'run-js)
  (when (and (fboundp 'lua-setup-keymap)
             (null lua-mode-map))
    (setq lua-mode-map (lua-setup-keymap)))

  (when (keymapp lua-mode-map)
    (keymap-set lua-mode-map "C-c C-r" #'lua-send-region)
    (keymap-set lua-mode-map "C-c C-b" #'lua-send-buffer)
    (keymap-set lua-mode-map "C-c C-l"
                #'lua-send-current-line)
    (keymap-set lua-mode-map "C-c C-e"
                #'lua-send-lua-region)))

(when (locate-library "lua-mode")
  (require 'lua-mode))
