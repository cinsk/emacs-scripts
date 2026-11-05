;; -*- mode: emacs-lisp; lexical-binding: t; -*-


;;;
;;; Lisp (Emacs Lisp, Common Lisp, slime, and Scheme) configuration
;;;

(defun syntactic-close-or-self-insert (&optional arg)
  "Call `syntactic-close' and if there is no matching character,
insert the character the user typed by calling `self-insert-command'."
  (interactive "P")
  (unless (call-interactively 'syntactic-close)
    (call-interactively 'self-insert-command nil
                        [(listify-key-sequence (this-command-keys))])))

(defun my/register-syntactic-close-binding (map)
  (keymap-set map ")" #'syntactic-close-or-self-insert)
  (keymap-set map "]" #'syntactic-close-or-self-insert)
  (keymap-set map "}" #'syntactic-close-or-self-insert)
  (keymap-set map ">" #'syntactic-close-or-self-insert))


(when (locate-library "syntactic-close")
  ;; '(lisp-interaction-mode-map emacs-lisp-mode-map lisp-mode-map)
  (my/register-syntactic-close-binding lisp-mode-shared-map)

  (with-eval-after-load "racket-mode"
    (my/register-syntactic-close-binding racket-mode-map))
  (with-eval-after-load "geiser-repl"
    (my/register-syntactic-close-binding geiser-repl-mode-map))
  (with-eval-after-load "scheme"
    (my/register-syntactic-close-binding scheme-mode-map))
  (with-eval-after-load "clojure-mode"
    (my/register-syntactic-close-binding clojure-mode-map)))

(with-eval-after-load "lisp-mode"
  (keymap-set emacs-lisp-mode-map "<f5>" #'eval-buffer)
  (keymap-set emacs-lisp-mode-map "C-c |" #'eval-region))







;;;
;;; Emacs Lisp Mode
;;;

(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
             (my/visit-tags-table (file-name-concat user-emacs-directory
						    "TAGS.emacs") t)))

(with-eval-after-load "lisp-mode"
  (keymap-set emacs-lisp-mode-map "<f5>" #'eval-buffer)
  (keymap-set emacs-lisp-mode-map "C-c |" #'eval-region))


;;;
;;; Common Lisp Mode -- from clisp-2.38/editors.txt
;;;
;;; It seems that Emacs already have `lisp-eval-last-sexp' that has
;;; the same feature of `

;; clisp does not work with slime package for now -- cinsk
;;(setq inferior-lisp-program "clisp -I -q -E utf-8")
;;(setq inferior-lisp-program "sbcl")

(defun lisp-macroexpand-region (start end &optional and-go)
  "Macroexpand the current region in the inferior Lisp process.
Prefix argument means switch to the Lisp buffer afterwards."
  (interactive "r\nP")
  (comint-send-string
   (inferior-lisp-proc)
   (format "(macroexpand-1 (quote %s))\n"
           (buffer-substring-no-properties start end)))
  (if and-go (switch-to-lisp t)))


(defun lisp-macroexpand-sexp (&optional and-go)
  "Macroexpand the next sexp in the inferior Lisp process.
Prefix argument means switch to the Lisp buffer afterwards."
  (interactive "P")
  (lisp-macroexpand-region (point) (scan-sexps (point) 1) and-go))

(with-eval-after-load "inf-lisp"
  (keymap-set inferior-lisp-mode-map "C-x RET"
              'lisp-macro-expand-sexp))

(keymap-set lisp-mode-map "C-x RET" #'lisp-macro-expand-sexp)


;;;
;;; slime
;;;
(when (locate-library "slime-autoloads")
  (with-eval-after-load "slime"
    (slime-setup)
    ;; C-c C-b slime-eval-buffer
    ;; C-c C-e slime-eval-last-expression
    ;; C-c C-r slime-eval-region

    ;; `M-x slime-interrupt' moved to `C-c C-B' from `C-c C-b'
    (my/move-key :keymap slime-mode-map :from "C-c C-b" :to "C-c C-B")
    (my/move-key :keymap slime-mode-map :from "C-c C-e" :to "C-M :")
    ;; C-c v   slime-describe-symbol
    ;; C-c f   slime-describe
    ;;(keymap-set slime-mode-map "C-c v" #'slime-describe-symbol)
    ;;(keymap-set slime-mode-map "C-c f" #'slime-describe-function)
    (keymap-set slime-mode-map "C-c C-e"
                #'slime-eval-last-expression)
    (keymap-set slime-mode-map "C-c C-b"
                #'slime-eval-buffer))
  (require 'slime-autoloads))


;; clisp does not work with slime package for now -- cinsk
;;(setq inferior-lisp-program "clisp -I -q -E utf-8")
(if (locate-file "sbcl" exec-path)
    (setq inferior-lisp-program "sbcl"))



;;;
;;; quack (enhanced support for scheme-mode)
;;;
(when (locate-library "quack")
  (require 'quack)
  (setq quack-browse-url-browser-function 'quack-w3m-browse-url-other-window)
  (setq quack-fontify-style 'emacs)
  (setq quack-default-program "mzscheme"))

(defun scheme-grep-symbols-on-region ()
  "Insert all global symbols into the selected buffer"
  (interactive)
  (let ((src (current-buffer))
        (dst (get-buffer-create "*scheme-tmp*"))
        (begin (region-beginning))
        (end (region-end)))
    (save-excursion
      (set-buffer dst)
      (erase-buffer)
      (scheme-mode)
      (insert "(provide "))
    (save-excursion
      (narrow-to-region begin end)
      (goto-char (point-min))
      (while (re-search-forward
              "(define[ \t\v\n]+(?[ \t\v\n]*\\([^ )\t\v\n]*\\)" nil t)
        (let ((word (match-string-no-properties 1)))
          (set-buffer dst)
          (insert word)
          ;;(indent-according-to-mode)
          ;;(newline)
          (newline-and-indent)
          (set-buffer src))))
    ))


(when (locate-library "eldoc")
  (require 'eldoc)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(when (and nil (locate-library "paredit"))
  (autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code." t)

  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

  ;; See http://www.emacswiki.org/emacs/ParEdit
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))





(with-eval-after-load "racket-mode"
  (defun my/racket-mode-switch-to-repl (&optional arg)
    (interactive "P")
    (if arg
        (racket-run-and-switch-to-repl)
      (let ((sessionp (and (get-buffer racket-repl-buffer-name)
                           (with-current-buffer (get-buffer racket-repl-buffer-name)
                             (racket--repl-session-id)))))
        (if sessionp
            (racket-edit-switch-to-repl)
          (racket-run-and-switch-to-repl)))))

  ;; TODO: `racket-run' or `racket-repl'?
  (keymap-set racket-mode-map "C-c !" #'my/racket-mode-switch-to-repl)
  ;; keep `C-c C-c' for `comment-region'.
  (my/move-key :keymap racket-mode-map :from "C-c C-c" :to "C-c C-C")

  ;; geiser forces to use scheme-mode for ".rkt" files. force to use
  ;; `racket-mode'.
  ;;
  ;; (my/priortize-auto-mode 'racket-mode)
  )

(when nil
(when (locate-library "geiser")
  (when (locate-library "racket-mode")
    (add-hook 'racket-mode-hook #'(lambda () (geiser-mode 1)))

    ;; geiser-mode made .rkt file uses scheme-mode.  Force to use
    ;; racket-mode if it is installed
    (my/priortize-auto-mode 'racket-mode)))

(with-eval-after-load "racket-mode"
  (when (locate-library "geiser")
    ;; `C-c C-c' bound to `racket-run`, which is unnecessary since we
    ;; are going to use geiser's `geiser-mode-switch-to-repl' to
    ;; create the REPL.  Also, I am used to use `C-c C-c' as
    ;; `comment-region'.
    (keymap-set racket-mode-map "C-c C-c" nil)))

(with-eval-after-load "geiser-mode"
  (keymap-set geiser-mode-map "C-c !" #'geiser-mode-switch-to-repl)
  (my/move-key :keymap geiser-mode-map :from "C-c C-c" :to "C-c C-C"))
)

(defun pop-to-scratch-buffer ()
  (interactive)
  (pop-to-buffer (get-buffer "*scratch*") nil 'norecord))

(keymap-global-set "C-x w s" #'pop-to-scratch-buffer)
