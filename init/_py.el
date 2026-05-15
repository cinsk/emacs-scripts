;; -*-emacs-lisp-*-

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


(defun pandas-set-display (&optional arg)
  "Adjust the display configuration of Pandas module."
  (interactive "P")
  (let* ((win (selected-window))
         (width (window-body-width win))
         (header "
if 'pd' in globals():
  if hasattr(pd, 'DataFrame'):
    globals()['__EMACS_PD__'] = pd
  else:
    import pandas
    globals()['__EMACS_PD__'] = pandas
else:
  import pandas as pd
  globals()['__EMACS_PD__'] = pd
")
         (footer "del __EMACS_PD__\n")
         (code (format "pd.options.display.width = %d\n" width)))
    (if arg
        (setq code (concat code "__EMACS_PD__.options.display.max_columns = None\n__EMACS_PD__.options.display.max_rows = None\n")))
    (python-shell-send-string (concat header code footer))))


(with-eval-after-load "pyenv-mode"
  ;; pyenv uses "C-c C-s" for `pyenv-mode-set', which overlapped with
  ;; python.el's "C-c C-s", `python-shell-send-string'.
  (define-key pyenv-mode-map [(control ?c) (control ?s)] nil)
  (define-key pyenv-mode-map [(control ?c) (control ?p)] 'pyenv-mode-set))

(when (pyenv-check-executable (executable-find "python"))
  (require 'pyenv-mode))

(with-eval-after-load "python"
  (add-to-list 'python-mode-hook 'pyenv-mode)


  (define-key python-mode-map [(control ?c) ?\!] 'run-python)

  ;; By default, python.el set "C-c C-c" to python-shell-send-buffer,
  ;; which overlapped with my personal config. (comment-region).
  (define-key python-mode-map [(control ?c) (control ?c)] nil)
  (define-key python-mode-map [(control ?c) (control ?b)] 'python-shell-send-buffer)

  ;; From Python 3.11, it supports terminal capability which is not
  ;; compatible with Emacs.  This forces Python to use legacy REPL, to
  ;; remove the 'pyrepl' warning.
  (setenv "PYTHON_BASIC_REPL" "1")

  (define-key inferior-python-mode-map [(control ?c) (control ?q)] 'pandas-set-display))
