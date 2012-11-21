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
;;(autoload 'python-mode "python-mode" "Python editing mode." t))

(eval-after-load "python-mode"
  '(progn
     ;; C-c C-b py-execute-buffer
     ;; C-c C-r py-execute-region
     ;; C-c C-e py-execute-string
     ;;
     ;; C-c C-c py-comment-region
     ;; C-c C-i py-indent-region
     ;;
     ;; C-c [   py-shift-region-left
     ;; C-c ]   py-shift-region-right

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

       (if (and (executable-find "ipython")
                (fboundp 'py-execute-region-ipython))
           (progn
             (setq-default py-shell-name "ipython")
             (define-key map [(control ?c) (control ?b)]
               'py-execute-buffer-ipython)
             (define-key map [(control ?c) (control ?r)]
               'py-execute-region-ipython)
             ;; py-execute-string-ipython is not provided yet
             ;; (python-mode 6.0.10)
             (define-key map [(control ?c) (control ?e)]
               'py-execute-string))
         (progn
           (define-key map [(control ?c) (control ?b)]
             'py-execute-buffer)
           (define-key map [(control ?c) (control ?r)]
             'py-execute-region)
           (define-key map [(control ?c) (control ?e)]
             'py-execute-string)))

       (define-key map [(control ?c) (control ?c)] 'py-comment-region)
       (define-key map [(control ?c) ?i] 'py-indent-region)

       (when (locate-file "pychecker" exec-path)
         (define-key map [(control ?c) ?c] 'py-pychecker-run))

       ;; python-mode uses `C-c C-d' for `py-pdbtrack-toggle-stack-tracking'
       (define-key map [(control ?c) (control ?d)] 'zap-to-nonspace))))

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(require 'python-mode)

;; ipython.el does not work with python-mode any longer. And since
;; python-mode provides an interface to ipython, I'll stick to
;; python-mode only from now on. -- cinsk
