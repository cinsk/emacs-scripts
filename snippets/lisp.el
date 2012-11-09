;; -*-emacs-lisp-*-


;;;
;;; Lisp (Emacs Lisp, Common Lisp, slime, and Scheme) configuration
;;;

;;;
;;; Emacs Lisp Mode
;;;

(add-hook 'emacs-lisp-mode-hook 
          '(lambda ()
             (safe-visit-tags-table (concat (file-name-as-directory 
                                             user-emacs-directory)
                                            "TAGS.emacs") t)))

(eval-after-load "lisp-mode"
  '(progn
     (define-key emacs-lisp-mode-map [f5] 'eval-buffer)
     (define-key emacs-lisp-mode-map [(control c) ?\|] 'eval-region)))


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

(eval-after-load "inf-lisp"
  '(define-key inferior-lisp-mode-map [(control ?x) (control ?m)] 
     'lisp-macro-expand-sexp))

(define-key lisp-mode-map [(control ?x) (control ?m)] 'lisp-macro-expand-sexp)


;;;
;;; slime
;;;
(when (locate-library "slime-autoloads")
  (eval-after-load "slime" 
    '(progn 
       (slime-setup)
       ;; C-c C-b slime-eval-buffer
       ;; C-c C-e slime-eval-last-expression
       ;; C-c C-r slime-eval-region

       ;; `M-x slime-interrupt' moved to `C-c C-B' from `C-c C-b'
       (move-key slime-mode-map [(control ?c) (control ?b)]
                 [(control ?c) (control ?B)])
       (move-key slime-mode-map [(control ?c) (control ?e)]
                 [(control meta ?\:)])
       ;; C-c v   slime-describe-symbol
       ;; C-c f   slime-describe
       ;;(define-key slime-mode-map [(control ?c) ?v] 'slime-describe-symbol)
       ;;(define-key slime-mode-map [(control ?c) ?f] 'slime-describe-function)
       (define-key slime-mode-map [(control ?c) (control ?e)]
         'slime-eval-last-expression)
       (define-key slime-mode-map [(control ?c) (control ?b)]
         'slime-eval-buffer)))
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

