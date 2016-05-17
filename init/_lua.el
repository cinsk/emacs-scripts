;; -*-emacs-lisp-*-

;;;
;;; lua configuration
;;;


(with-eval-after-load "lua-mode"
  ;;(define-key lua-mode-map [(control ?c) ?\!] 'run-js)
  (when (and (fboundp 'lua-setup-keymap)
             (null lua-mode-map))
    (setq lua-mode-map (lua-setup-keymap)))

  (when (keymapp lua-mode-map)
    (define-key lua-mode-map [(control ?c) (control ?r)] 'lua-send-region)
    (define-key lua-mode-map [(control ?c) (control ?b)] 'lua-send-buffer)
    (define-key lua-mode-map [(control ?c) (control ?l)]
      'lua-send-current-line)
    (define-key lua-mode-map [(control ?c) (control ?e)]
      'lua-send-lua-region)))

(when (locate-library "lua-mode")
  (require 'lua-mode))
