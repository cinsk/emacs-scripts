;; -*- mode: emacs-lisp; lexical-binding: t; -*-


(defmacro enable-minor-mode (minor-mode &rest args)
  "Enable MINOR-MODE on a major mode, optionally based on a condition.

ARGS is a sequence of keyword/value pairs, optionally followed by
condition forms:

  :major-mode MODE  Major mode symbol.  Used to derive :file and
                    :hook when those are omitted.
  :file FILE        Library name passed to `locate-library' and
                    `with-eval-after-load'.  Defaults to the name
                    of :major-mode.
  :hook HOOK        Hook symbol to add to.  Defaults to
                    MODE-hook, where MODE is :major-mode (or :file).
  :feature FEATURE  Feature to `require' before enabling.
                    Defaults to MINOR-MODE.
  :verbose FORM     If FORM evaluates to non-nil, report a missing
                    major mode library in the *Warnings* buffer.
  :when CONDITION   A single condition (see below).  Alternatively,
                    condition forms may follow the keywords as a body.

If no condition is given, MINOR-MODE is always enabled.  If the
condition is a single string literal, it is treated as a regexp
matched against `buffer-file-name'.  Otherwise the condition forms
are evaluated as a `progn' and MINOR-MODE is enabled when the
result is non-nil.

Since the hook may not exist when this macro is evaluated, the
code is wrapped in `with-eval-after-load' on :file.

Error handling:
- If :file cannot be located, nothing happens, unless :verbose
  is non-nil, in which case a warning is displayed.
- If :feature cannot be loaded, an error is always reported in
  the *Warnings* buffer, once the major mode library is loaded.

Examples:

  (enable-minor-mode skewer-mode
    :major-mode js2-mode)

  (enable-minor-mode skewer-mode
    :major-mode js2-mode
    :verbose t
    :when \"/d3/.*\\\\.js\\\\\\='\")

  (enable-minor-mode skewer-mode
    :major-mode js2-mode
    :when (and buffer-file-name
               (string-prefix-p (expand-file-name \"~/src/js/d3/\")
                                buffer-file-name)))"
  (declare (indent 1))
  (let (major file hook feature verbose condition when-given)
    ;; Parse leading keyword/value pairs.
    (while (keywordp (car args))
      (let ((key (pop args)))
        (unless args
          (error "enable-minor-mode: missing value for %S" key))
        (let ((val (pop args)))
          (pcase key
            (:major-mode (setq major val))
            (:file       (setq file val))
            (:hook       (setq hook val))
            (:feature    (setq feature val))
            (:verbose    (setq verbose val))
            (:when       (setq condition (list val) when-given t))
            (_ (error "enable-minor-mode: unknown keyword %S" key))))))
    ;; Anything left is the condition body.
    (when args
      (when when-given
        (error "enable-minor-mode: use either :when or a condition body, not both"))
      (setq condition args))
    ;; Defaults.
    (unless file
      (if major
          (setq file (symbol-name major))
        (error "enable-minor-mode: :file or :major-mode is required")))
    (unless hook
      (setq hook (intern (format "%s-hook" (or major file)))))
    (unless feature
      (setq feature minor-mode))
    (let ((test (cond ((null condition) t)
                      ((and (null (cdr condition)) (stringp (car condition)))
                       `(and buffer-file-name
                             (string-match-p ,(car condition) buffer-file-name)))
                      (t `(progn ,@condition)))))
      `(if (locate-library ,file)
           (with-eval-after-load ,file
             (when (condition-case err
                       (progn (require ',feature) t)
                     (error
                      (display-warning
                       'enable-minor-mode
                       (format "Cannot load `%s' for `%s': %s"
                               ',feature ',minor-mode
                               (error-message-string err))
                       :error)
                      nil))
               (add-hook ',hook
                         (lambda ()
                           (when ,test
                             (,minor-mode 1))))))
         ,(when verbose
            `(when ,verbose
               (display-warning
                'enable-minor-mode
                (format "Cannot find library `%s' for `%s'"
                        ,file ',minor-mode)
                :error)))))))

(provide 'enable-mmode)
