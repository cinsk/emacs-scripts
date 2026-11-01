;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; uinit.el --- a use-package-like loader for personal init snippets

;;; Commentary:
;;
;; `use-package' is great for configuring installed Emacs *packages*,
;; but this configuration is also split into a bunch of small, modular
;; init *snippets* (plain .el files under `uinit-init-directory', e.g.
;; init/_org.el, init/darwin.el, ...) that are not packages at all.
;; `uinit' plays that role, with an interface modeled after
;; `use-package':
;;
;;   (uinit "_org"
;;     :when (locate-library "org"))
;;
;;   (uinit "_lua"
;;     :when (locate-library "lua-mode")
;;     :init (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;;           (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))
;;
;;   (uinit "wsl" :when (wsl-p))
;;
;; Keywords:
;;
;;   :when EXPR / :if EXPR   Load the snippet only if EXPR is non-nil.
;;                           EXPR is evaluated every time the form runs
;;                           (the default, if omitted, is to load
;;                           unconditionally).  May be given more than
;;                           once; all of them are ANDed together.
;;   :unless EXPR            Load only if EXPR is nil.  Combines with
;;                           :when/:if.
;;   :disabled               Never load (the form is still recorded by
;;                           `uinit-report' as "disabled"), regardless
;;                           of :when/:unless.
;;   :init BODY...           Forms run once, before the snippet file is
;;                           loaded (only when the condition above is
;;                           satisfied).
;;   :config BODY...         Forms run once, right after the snippet
;;                           file has been loaded successfully.
;;
;; For requiring an already-installed feature (rather than loading a
;; file from `uinit-init-directory') with the same statistics
;; bookkeeping, use `uinit-require', e.g. (uinit-require 'dwim-compile).
;;
;; Once Emacs has started, run `M-x uinit-report' to see which
;; snippets were loaded, skipped, disabled, or errored out, and how
;; long each one took -- the `uinit' equivalent of `use-package-report'.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(eval-when-compile (require 'subr-x))

(defgroup uinit nil
  "A use-package-like loader for personal init snippets."
  :group 'initialization)

(defcustom uinit-init-directory
  (expand-file-name "init" user-emacs-directory)
  "Directory that contains the modular init snippets loaded by `uinit'."
  :type 'directory
  :group 'uinit)

(defcustom uinit-byte-compile-directories
  (list uinit-init-directory
        (expand-file-name "src" user-emacs-directory))
  "Directories that are byte-recompiled when this library loads.
See `uinit-byte-compile-on-load'."
  :type '(repeat directory)
  :group 'uinit)

(defcustom uinit-byte-compile-on-load t
  "If non-nil, byte-recompile `uinit-byte-compile-directories' on load.

This can be overridden at Emacs startup with the EMACS_COMPILE
environment variable (\"0\" forces this off, anything else forces it
on)."
  :type 'boolean
  :group 'uinit)

(defvar uinit-debug-nop nil
  "If non-nil, `uinit' and `uinit-require' only record statistics.

No snippet file or feature is actually loaded.  Useful to quickly see
what *would* be loaded.  Enabled automatically when the EMACS_NOP
environment variable is set at startup.")

(defvar uinit-statistics nil
  "List of plists describing every `uinit'/`uinit-require' form run so far.

Newest first.  Each plist has the keys :name, :file, :status, :time,
and :condition.  STATUS is one of `loaded', `already-loaded',
`skipped', `disabled', or `error'.  See `uinit-report'.")

(defvar uinit--loaded-files nil
  "Absolute file names that have already been loaded via `uinit'.")

(defconst uinit--start-time (current-time)
  "Time at which this library -- and hence init snippet loading -- started.")

(defun uinit--resolve-file (name)
  "Return the absolute file name of the init snippet NAME."
  (if (file-name-absolute-p name)
      name
    (expand-file-name name uinit-init-directory)))

(defun uinit--record (name file status elapsed condition)
  "Push a new statistics entry onto `uinit-statistics'."
  (push (list :name name :file file :status status :time elapsed
              :condition condition)
        uinit-statistics))

;;;###autoload
(defmacro uinit (name &rest args)
  "Conditionally load the init snippet NAME, recording load statistics.

NAME is the base file name (a string) of a snippet under
`uinit-init-directory'; an absolute file name is used as-is.  ARGS is
a sequence of keywords as described in the Commentary section of
uinit.el, namely :when/:if, :unless, :disabled, :init, and :config.

\(fn NAME [:when EXPR] [:unless EXPR] [:disabled] [:init BODY...] [:config BODY...])"
  (declare (indent 1) (debug (form &rest sexp)))
  (let (conditions init-forms config-forms disabled)
    (while (keywordp (car args))
      (pcase (pop args)
        ((or :when :if) (push (pop args) conditions))
        (:unless (push `(not ,(pop args)) conditions))
        (:disabled (setq disabled t))
        (:init
         (while (and args (not (keywordp (car args))))
           (push (pop args) init-forms)))
        (:config
         (while (and args (not (keywordp (car args))))
           (push (pop args) config-forms)))
        (key (error "uinit: unknown keyword %s" key))))
    (setq conditions (nreverse conditions)
          init-forms (nreverse init-forms)
          config-forms (nreverse config-forms))
    `(uinit--run ,name ,disabled
                 (lambda () ,(if conditions `(and ,@conditions) t))
                 ',(or conditions t)
                 (lambda () ,@init-forms)
                 (lambda () ,@config-forms))))

(defun uinit--run (name disabled test-fn condition-form init-fn config-fn)
  "Runtime helper backing the `uinit' macro; see its docstring."
  (let* ((file (uinit--resolve-file name))
         (started (current-time))
         status)
    (condition-case err
        (cond
         (disabled (setq status 'disabled))
         ((member file uinit--loaded-files) (setq status 'already-loaded))
         ((not (funcall test-fn)) (setq status 'skipped))
         (uinit-debug-nop (setq status 'nop))
         (t
          (funcall init-fn)
          (load file)
          (push file uinit--loaded-files)
          (funcall config-fn)
          (setq status 'loaded)))
      (error
       (setq status 'error)
       (lwarn 'uinit :warning "%s: %s" name (error-message-string err))))
    (uinit--record name file status
                   (float-time (time-subtract (current-time) started))
                   condition-form)
    (eq status 'loaded)))

(defun uinit-require (feature &optional filename noerror)
  "Like `require', but record statistics the way `uinit' does.

Returns non-nil if FEATURE ends up provided."
  (let* ((name (symbol-name feature))
         (file (or (locate-library name) name))
         (started (current-time))
         status)
    (condition-case err
        (if (featurep feature)
            (setq status 'already-loaded)
          (unless uinit-debug-nop
            (require feature filename noerror))
          (setq status (if (featurep feature) 'loaded 'skipped)))
      (error
       (setq status 'error)
       (unless noerror
         (lwarn 'uinit :warning "%s: %s" feature (error-message-string err)))))
    (uinit--record name file status
                   (float-time (time-subtract (current-time) started))
                   nil)
    (featurep feature)))

;;;
;;; Reporting -- `M-x uinit-report', modeled after `use-package-report'.
;;;

(defun uinit-report--status-face (status)
  (pcase status
    ('loaded 'success)
    ('already-loaded 'shadow)
    ('error 'error)
    ('disabled 'shadow)
    ('skipped 'warning)
    ('nop 'warning)
    (_ 'default)))

(defun uinit-report--entries ()
  (mapcar
   (lambda (entry)
     (let ((status (plist-get entry :status)))
       (list (plist-get entry :file)
             (vector
              (propertize (symbol-name status)
                          'face (uinit-report--status-face status))
              (format "%.4f" (plist-get entry :time))
              (plist-get entry :name)
              (let ((c (plist-get entry :condition)))
                (if (or (null c) (eq c t)) "" (format "%S" c)))))))
   (reverse uinit-statistics)))

(defvar uinit-report-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (keymap-set map "RET" #'uinit-report-find-file)
    (keymap-set map "o" #'uinit-report-find-file-other-window)
    map))

(define-derived-mode uinit-report-mode tabulated-list-mode "Uinit-Report"
  "Major mode listing every `uinit'/`uinit-require' form run so far."
  (setq tabulated-list-format
        [("Status" 14 t)
         ("Time (s)" 10 uinit-report--time-predicate)
         ("Name" 20 t)
         ("Condition" 0 nil)])
  (setq tabulated-list-sort-key (cons "Time (s)" t))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun uinit-report--time-predicate (a b)
  (< (string-to-number (aref (cadr a) 1))
     (string-to-number (aref (cadr b) 1))))

(defun uinit-report--summary ()
  (let ((total 0.0) (counts (make-hash-table :test #'eq)))
    (dolist (entry uinit-statistics)
      (cl-incf total (plist-get entry :time))
      (cl-incf (gethash (plist-get entry :status) counts 0)))
    (format "%d form(s), %.4fs total (%.4fs wall since uinit loaded) -- %s"
            (length uinit-statistics) total
            (float-time (time-subtract (current-time) uinit--start-time))
            (mapconcat (lambda (s) (format "%s: %d" (car s) (cdr s)))
                       (let (acc) (maphash (lambda (k v) (push (cons k v) acc)) counts) acc)
                       ", "))))

(defun uinit-report-find-file ()
  "Visit the init snippet file at point."
  (interactive)
  (let ((file (tabulated-list-get-id)))
    (if (and file (file-exists-p file))
        (find-file file)
      (user-error "No file associated with this entry"))))

(defun uinit-report-find-file-other-window ()
  "Visit the init snippet file at point, in another window."
  (interactive)
  (let ((file (tabulated-list-get-id)))
    (if (and file (file-exists-p file))
        (find-file-other-window file)
      (user-error "No file associated with this entry"))))

;;;###autoload
(defun uinit-report ()
  "Show a report of every `uinit'/`uinit-require' form run so far.

Lists each init snippet's load status and the time it took, sorted by
time descending (press RET to visit the snippet, `o' for another
window).  This is the `uinit' equivalent of `use-package-report'."
  (interactive)
  (let ((buf (get-buffer-create "*Uinit Report*")))
    (with-current-buffer buf
      (uinit-report-mode)
      (setq tabulated-list-entries (uinit-report--entries))
      (setq header-line-format (uinit-report--summary))
      (tabulated-list-print t))
    (pop-to-buffer buf)))

;;;
;;; Byte-compile init snippets & this package on load.
;;;

(let* ((env-compile (getenv "EMACS_COMPILE"))
       (do-compile (if (member env-compile '(nil ""))
                       uinit-byte-compile-on-load
                     (not (equal env-compile "0")))))
  (when (getenv "EMACS_NOP")
    (setq uinit-debug-nop t))
  (when do-compile
    (dolist (dir uinit-byte-compile-directories)
      (let ((d (expand-file-name dir)))
        (when (file-directory-p d)
          (byte-recompile-directory d 0))))))

(provide 'uinit)

;;; uinit.el ends here
