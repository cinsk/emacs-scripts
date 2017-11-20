;; -*-emacs-lisp-*-

;;;
;;; Shell configuration
;;;
(eval-when-compile
  (require 'eshell)
  (require 'term))

;;
;; Make the inferior shell a login shell.
;;
(setq explicit-bash-args '("--noediting" "-i" "-l"))

;; `shell' runs an inferior shell in ASCII coding system.
;; `unicode-shell' behaves the same as `shell' except it runs an inferior
;; shell in UTF-8 coding system.
(defun unicode-shell (&optional encoding)
  "Execute the shell buffer in UTF-8 encoding.
Note that you'll need to set the environment variable LANG and others
appropriately."
  (interactive)
  (let ((coding-system-for-read (or encoding 'utf-8))
        (coding-system-for-write (or encoding 'utf-8))
        (coding-system-require-warning t))
    (call-interactively 'shell)))

;; Allow shell mode to handle color output from shell commands
;; (notably from ls --color command)
;;
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;
;; `term/shell' is similar to `shell' based on `ansi-term' code.
;;
(defun term/shell (program &optional new-buffer-name)
  "Start a terminal-emulator in a new buffer.

With a prefix argument, it prompts the user for the shell
executable.

If there is already existing buffer with the same name, switch to
that buffer, otherwise it creates new buffer.

Like `shell', it loads `~/.emacs_SHELLNAME' if exists, or
`~/.emacs.d/init_SHELLNAME.sh'.

The shell file name (sans directories) is used to make a symbol
name such as `explicit-bash-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the
shell."
  (interactive (let ((default-prog (or explicit-shell-file-name
                                       (getenv "ESHELL")
                                       shell-file-name
                                       (getenv "SHELL")
                                       "/bin/sh")))
                 (list (if (or (null default-prog)
                               current-prefix-arg)
                           (read-from-minibuffer "Run program: " default-prog)
                         default-prog))))

  (require 'term)
  ;; Pick the name of the new buffer.
  (setq term-ansi-buffer-name
        (if new-buffer-name
            new-buffer-name
          (if term-ansi-buffer-base-name
              (if (eq term-ansi-buffer-base-name t)
                  (file-name-nondirectory program)
                term-ansi-buffer-base-name)
            "shell/term")))

  (setq term-ansi-buffer-name (concat "*" term-ansi-buffer-name "*"))

  ;; In order to have more than one term active at a time
  ;; I'd like to have the term names have the *term-ansi-term<?>* form,
  ;; for now they have the *term-ansi-term*<?> form but we'll see...
  (when current-prefix-arg
    (setq term-ansi-buffer-name
          (generate-new-buffer-name term-ansi-buffer-name)))

  (let* ((name (file-name-nondirectory program))
         (startfile (concat "~/.emacs_" name))
         (xargs-name (intern-soft (concat "explicit-" name "-args"))))
    (unless (file-exists-p startfile)
      (setq startfile (concat user-emacs-directory "init_" name ".sh")))

    (setq term-ansi-buffer-name
          (apply 'term-ansi-make-term term-ansi-buffer-name program
                 (if (file-exists-p startfile) startfile)
                 (if (and xargs-name (boundp xargs-name))
                     ;; `term' does need readline support.
                     (remove "--noediting" (symbol-value xargs-name))
                   '("-i")))))

  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-char-mode)

  ;; I wanna have find-file on C-x C-f -mm
  ;; your mileage may definitely vary, maybe it's better to put this in your
  ;; .emacs ...

  (term-set-escape-char ?\C-x)

  (switch-to-buffer term-ansi-buffer-name))

(global-set-key "\C-cd" 'shell)

;; TODO: implement generic function to send arbitrary command to eshell.
;; (require 'eshell)

(defun eshell-cd (&optional dir)
  "Switch to eshell and cd to DIR.

If DIR is nil or called interactively, `default-directory' will
be used.  If a prefix argument given, it will prompt for the
directory it will use.

This function is stealed from `helm-ff-switch-to-eshell'."
  (interactive (list (if current-prefix-arg
                         (read-directory-name "dir: ")
                       nil)))
  (or dir (setq dir default-directory))
  (let ((cd-eshell #'(lambda ()
                       (eshell-kill-input)
                       (goto-char (point-max))
                       (insert
                        (format "cd '%s'" dir))
                       (eshell-send-input))))
    (if (get-buffer "*eshell*")
        ;; pop-to-buffer?
        ;; (switch-to-buffer "*eshell*")
        (pop-to-buffer "*eshell*")
      (call-interactively 'eshell))
    (unless (get-buffer-process (current-buffer))
      (funcall cd-eshell))))

(global-set-key [(control meta ?\!)] 'eshell-cd)

;;(require 'eshell)
;;(require 'em-term)
(with-eval-after-load "em-term"
    (add-to-list 'eshell-visual-commands "vim"))

;; (require 'em-smart)
;; (setq eshell-where-to-jump 'begin)
;; (setq eshell-review-quick-commands nil)
;; (setq eshell-smart-space-goes-to-end t)
(setq shell-pushd-regexp "\\(pushd\\|c\\)"
      shell-popd-regexp "\\(popd\\|p\\)")

;;
;; From NEWS Emacs version 25
;;
;; When you invoke 'shell' interactively, the '*shell*' buffer will now
;; display in a new window.  However, you can customize this behavior via
;; the 'display-buffer-alist' variable.  For example, to get
;; the old behavior -- '*shell*' buffer displays in current window -- use
;; (add-to-list 'display-buffer-alist
;;      '("^\\*shell\\*$" . (display-buffer-same-window))).
(add-to-list 'display-buffer-alist
             '("^\\*shell\\*$" . (display-buffer-same-window)))

(with-eval-after-load "shell"
  (cinsk/move-key shell-mode-map [(control ?c) (control ?o)] [(control ?c) (control ?O)]))
