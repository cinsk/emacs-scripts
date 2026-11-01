;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;
;;; python mode configuration (python.el from vanilla Emacs)
;;;

(eval-when-compile
  (require 'subr-x))

(defun pyenv-check-executable (snake)
  "Return t if SNAKE is under the path of pyenv(1) environment.

SNAKE should be a string in absolute path."
  (let (out)
    (setq out (string-trim (with-output-to-string
                             (with-temp-buffer
                               (let ((tmpbuf (current-buffer)))
                                 (with-current-buffer
                                     standard-output
                                   (shell-command "pyenv root" t tmpbuf)))))))
    (not (not (string-match-p (concat "^" (regexp-quote out))
                              snake)))))

(with-eval-after-load "pyenv-mode"
  ;; pyenv uses "C-c C-s" for `pyenv-mode-set', which overlapped with
  ;; python.el's "C-c C-s", `python-shell-send-string'.
  (keymap-set pyenv-mode-map "C-c C-s" nil)
  (keymap-set pyenv-mode-map "C-c C-p" #'pyenv-mode-set))

(when (pyenv-check-executable (executable-find "python"))
  (require 'pyenv-mode))

(setq python-shell-setup-codes '("\
import os
_fname = os.path.expanduser('~/.emacs.d/init.py')
if os.access(_fname, os.R_OK):
  exec(open(_fname).read())
  print(f'loaded: {_fname}')
del _fname"))

(require 'pandas)

(with-eval-after-load "python"
  (add-to-list 'python-mode-hook 'pyenv-mode)


  (keymap-set python-mode-map "C-c !" #'run-python)

  ;; By default, python.el set "C-c C-c" to python-shell-send-buffer,
  ;; which overlapped with my personal config. (comment-region).
  (keymap-set python-mode-map "C-c C-c" nil)
  (keymap-set python-mode-map "C-c C-b" #'python-shell-send-buffer)

  ;; From Python 3.11, it supports terminal capability which is not
  ;; compatible with Emacs.  This forces Python to use legacy REPL, to
  ;; remove the 'pyrepl' warning.
  (setenv "PYTHON_BASIC_REPL" "1")

  (keymap-set inferior-python-mode-map "C-c C-q" #'pd-set-display)
  (keymap-set inferior-python-mode-map "C-c C-f C-p" #'pd-dataframe-from-clipboard)
  (keymap-set inferior-python-mode-map "C-c C-f C-o" #'pd-dataframe-from-orgtable)

  (keymap-global-set "C-x w p" #'python-shell-switch-to-shell)
  )

(use-package uv-mode
  :ensure nil
  :if (locate-library "uv-mode")
  :hook (python-mode . uv-mode-auto-activate-hook))
