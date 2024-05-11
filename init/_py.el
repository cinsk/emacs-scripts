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
  (define-key python-mode-map [(control ?c) (control ?b)] 'python-shell-send-buffer))
