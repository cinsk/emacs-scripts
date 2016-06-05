
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

  (move-key paredit-mode-map [(control ?k)] [(control ?K)])
  (move-key paredit-mode-map [(control ?d)] [(control ?D)])
  (move-key paredit-mode-map (kbd "DEL") [(shift backspace)]))


(when nil
  ;; (define-key smartparens-mode-map ([control ?d]) 'sp-delete-char)
  (define-key smartparens-mode-map [(control ?k)] 'sp-kill-sexp)
  (define-key smartparens-mode-map [(meta ?k)] 'sp-copy-sexp)
  (define-key smartparens-mode-map [(meta ?d)] 'sp-kill-word)

  (define-key smartparens-mode-map [(control meta ?f)] 'sp-forward-sexp)
  (define-key smartparens-mode-map [(control meta ?b)] 'sp-backward-sexp)
  (define-key smartparens-mode-map [(control meta ?n)] 'sp-up-sexp)
  (define-key smartparens-mode-map [(control meta ?p)] 'sp-down-sexp)


  (define-key smartparens-mode-map [(control ?\()] 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map [(control ?\))] 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map [(control ?\{)] 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map [(control ?\})] 'sp-forward-barf-sexp)

  (define-key smartparens-mode-map [(control meta left)] 'sp-beginning-of-sexp)
  (define-key smartparens-mode-map [(control meta right)] 'sp-end-of-sexp)

  (define-key smartparens-mode-map [(meta ?s)] 'sp-splice-sexp)
  (define-key smartparens-mode-map [(meta down)] 'sp-splice-sexp-killing-forward)
  (define-key smartparens-mode-map [(meta up)] 'sp-splice-sexp-killing-backward)
  )
