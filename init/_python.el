;; -*-emacs-lisp-*-

;;;
;;; python-mode configuration
;;;
;;; Note that this configuration is for `python-mode.el' not for
;;; `python.el' in GNU Emacs distribution.

;;;
;;; Note that `python-mode' configuration must come after `ruby-mode'
;;; configuration.  Otherwise, `python-mode' will set
;;; `ruby-indent-level' to nil, thus indentation of ruby code will
;;; failed.
;;;

;;
;; In python-mode 5.1.0, autoloading `python-mode' causes `eval-after-load'
;; failed.  Don't know why
;;
; (autoload 'python-mode "python-mode" "Python editing mode." t))

;;
;; Emacs 27.1, python-mode-20230702.625:
;; python-mode.el is not autoloaded unless we call explicit `require'.
;(require 'python-mode)
(autoload 'python-mode "python-mode.el" "Python Mode." t)

;;
;; From IPython 5, its prompt is not compatible with Emacs.
;; -- source: https://www.emacswiki.org/emacs/PythonProgrammingInEmacs
;;

(eval-when-compile
  (require 'subr-x))

(with-eval-after-load "python-mode"
  ;; C-c C-b py-execute-buffer
  ;; C-c C-r py-execute-region
  ;; C-c C-e py-execute-string
  ;;
  ;; C-c C-c py-comment-region
  ;; C-c C-i py-indent-region
  ;;
  ;; C-c [   py-shift-region-left
  ;; C-c ]   py-shift-region-right

  ;; Previously, py-ipython-command-args was a string type, but it
  ;; seems it is list of option strings at the moment.
  (unless (member "--simple-prompt" py-ipython-command-args)
    (add-to-list 'py-ipython-command-args "--simple-prompt"))

  (let ((map (if (boundp 'python-mode-map)
                 python-mode-map
               py-mode-map)))
    (define-key map [(control ?c) ?\]]
      'py-shift-region-right)
    (define-key map [(control ?c) ?\[]
      'py-shift-region-left)

    ;; To eval string/region/buffer in native python,
    ;; use py-execute-(string|region|buffer).
    ;;
    ;; To eval in ipython, use py-execute-(string|region|buffer)-ipython.

    (when nil
      (if (and (executable-find "ipython")
               (fboundp 'py-execute-region-ipython))
          (progn
            (setq py-start-run-ipython-shell t)
            (setq-default py-shell-name "ipython")

            ;; The default prompt of ipython is not working properly
            ;; it contains some ^A (ASCII 1) which makes completion
            ;; not working.
            (setq-default ;; py-python-command-args '("-i" "--classic")
             py-ipython-command-args "-i --classic")

            (define-key map [(control ?c) (control ?b)]
              'py-execute-buffer-ipython)
            (define-key map [(control ?c) (control ?r)]
              'py-execute-region-ipython)
            (define-key map [(control ?c) ?f]
              'py-execute-def-ipython)
            ;; py-execute-string-ipython is not provided yet
            ;; (python-mode 6.0.10)
            (define-key map [(control ?c) (control ?e)]
              'py-execute-string))))

    (progn
      (define-key map [(control ?c) (control ?b)]
        'py-execute-buffer)
      (define-key map [(control ?c) (control ?r)]
        'py-execute-region)
      (define-key map [(control ?c) ?f]
        'py-execute-def)
      (define-key map [(control ?c) (control ?e)]
        'py-execute-string))

    (define-key map [(control ?c) (control ?c)] 'py-comment-region)
    (define-key map [(control ?c) ?i] 'py-indent-region)

    (when (and (boundp 'py-shell-map)
               (null (lookup-key py-shell-map [(tab)]))
               (fboundp 'py-shell-complete))
      (define-key py-shell-map [(tab)] 'py-shell-complete))

    (when (locate-file "pychecker" exec-path)
      (define-key map [(control ?c) ?c] 'py-pychecker-run))

    ;; python-mode uses `C-c C-d' for `py-pdbtrack-toggle-stack-tracking'
    (define-key map [(control ?c) (control ?d)] 'zap-to-nonspace))

  (when (and (boundp 'py-shell-map)
             (null (lookup-key py-shell-map [(control ?a)])))
    ;; `move-beginning-of-line' (C-a) ignore the prompt, which is
    ;; inconvinient.
    (define-key py-shell-map [(control ?a)]
      'comint-bol-or-process-mark)))

;; (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;; (setq interpreter-mode-alist (cons '("python" . python-mode)
;;                                    interpreter-mode-alist))
;;

(setq py-indent-paren-spanned-multilines-p nil)

;; Aquamacs ships its own version of python-mode.
;;
;; TODO: do we need to check whether pytohn-mode provided by the
;; package system?


;; ipython.el does not work with python-mode any longer. And since
;; python-mode provides an interface to ipython, I'll stick to
;; python-mode only from now on. -- cinsk
