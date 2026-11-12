;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; C-c             Prefix Command
;; C-d             paredit-forward-delete
;; C-j             paredit-newline
;; C-k             paredit-kill
;; ESC             Prefix Command
;; "               paredit-doublequote
;; (               paredit-open-round
;; )               paredit-close-round
;; ;               paredit-semicolon
;; [               paredit-open-square
;; \               paredit-backslash
;; ]               paredit-close-square
;; DEL             paredit-backward-delete
;; C-(             paredit-backward-slurp-sexp
;; C-)             paredit-forward-slurp-sexp
;; C-{             paredit-backward-barf-sexp
;; C-}             paredit-forward-barf-sexp
;; <C-M-left>      paredit-backward-slurp-sexp
;; <C-M-right>     paredit-backward-barf-sexp
;; <C-left>        paredit-forward-barf-sexp
;; <C-right>       paredit-forward-slurp-sexp
;; <M-down>        paredit-splice-sexp-killing-forward
;; <M-up>          paredit-splice-sexp-killing-backward
;; <delete>        paredit-forward-delete
;; <deletechar>    paredit-forward-delete
;;
;; C-c ESC         Prefix Command
;;
;; C-M-b           paredit-backward
;; C-M-d           paredit-forward-down
;; C-M-f           paredit-forward
;; C-M-n           paredit-forward-up
;; C-M-p           paredit-backward-down
;; C-M-u           paredit-backward-up
;; M-"             paredit-meta-doublequote
;; M-(             paredit-wrap-round
;; M-)             paredit-close-round-and-newline
;; M-;             paredit-comment-dwim
;; M-?             paredit-convolute-sexp
;; M-J             paredit-join-sexps
;; M-S             paredit-split-sexp
;; M-d             paredit-forward-kill-word
;; M-q             paredit-reindent-defun
;; M-r             paredit-raise-sexp
;; M-s             paredit-splice-sexp
;; M-DEL           paredit-backward-kill-word
;; ESC <C-left>    paredit-backward-slurp-sexp
;; ESC <C-right>   paredit-backward-barf-sexp
;; ESC <down>      paredit-splice-sexp-killing-forward
;; ESC <up>        paredit-splice-sexp-killing-backward
;;
;; C-c C-M-l       paredit-recenter-on-sexp


(with-eval-after-load "paredit"
  (when (locate-library "eldoc")
    (require 'eldoc)
    ;; See http://www.emacswiki.org/emacs/ParEdit
    (eldoc-add-command
     'paredit-backward-delete
     'paredit-close-round))

  (my/move-key :keymap paredit-mode-map :from "C-k" :to "C-S-k")
  (my/move-key :keymap paredit-mode-map :from "C-d" :to "C-S-d") ; TODO: conflict with global key bindings
  (my/move-key :keymap paredit-mode-map :from "DEL" :to "S-<backspace>"))


(when nil
  ;; (keymap-set smartparens-mode-map "C-d" 'sp-delete-char)
  (keymap-set smartparens-mode-map "C-k" #'sp-kill-sexp)
  (keymap-set smartparens-mode-map "M-k" #'sp-copy-sexp)
  (keymap-set smartparens-mode-map "M-d" #'sp-kill-word)

  (keymap-set smartparens-mode-map "C-M-f" #'sp-forward-sexp)
  (keymap-set smartparens-mode-map "C-M-b" #'sp-backward-sexp)
  (keymap-set smartparens-mode-map "C-M-n" #'sp-up-sexp)
  (keymap-set smartparens-mode-map "C-M-p" #'sp-down-sexp)


  (keymap-set smartparens-mode-map "C-(" #'sp-backward-slurp-sexp)
  (keymap-set smartparens-mode-map "C-)" #'sp-forward-slurp-sexp)
  (keymap-set smartparens-mode-map "C-{" #'sp-backward-barf-sexp)
  (keymap-set smartparens-mode-map "C-}" #'sp-forward-barf-sexp)

  (keymap-set smartparens-mode-map "C-M-<left>" #'sp-beginning-of-sexp)
  (keymap-set smartparens-mode-map "C-M-<right>" #'sp-end-of-sexp)

  (keymap-set smartparens-mode-map "M-s" #'sp-splice-sexp)
  (keymap-set smartparens-mode-map "M-<down>" #'sp-splice-sexp-killing-forward)
  (keymap-set smartparens-mode-map "M-<up>" #'sp-splice-sexp-killing-backward)
  )
