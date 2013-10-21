;; -*-emacs-lisp-*-

;;;
;;; lua configuration
;;;


(eval-after-load "lua-mode"
  '(progn
     ;;(define-key lua-mode-map [(control ?c) ?\!] 'run-js)
     (define-key lua-mode-map [(control ?c) (control ?r)] 'lua-send-region)
     (define-key lua-mode-map [(control ?c) (control ?b)] 'lua-send-buffer)
     (define-key lua-mode-map [(control ?c) (control ?l)]
       'lua-send-current-line)
     (define-key lua-mode-map [(control ?c) (control ?e)] 'lua-send-lua-region)
     ))


(require 'lua-mode)
