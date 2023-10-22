(defvar uinit/time-started (current-time)
  "The time that Emacs start to load init files")

(defvar uinit/buffer-name "*uinit*"
  "The time that Emacs start to load init files")

(defvar uinit/loaded-init-files nil
  "List of loaded snippets.")

(defvar uinit/init-directory
  (concat (expand-file-name user-emacs-directory)
          "init")
  "Directory contains the init snippets.")

(defvar uinit/byte-compile-directories
  (list uinit/init-directory
        (concat (expand-file-name user-emacs-directory)
                "src"))
  "List of directories which are targets of byte compilations..")

(defvar uinit/use-byte-compile t
  "The time that Emacs start to load init files")

(defvar uinit/debug-nop nil
  "uinit will not load any submodules if non-nil")

(defmacro uinit/load (snippet &rest body)
  "If the last sexp of the BODY results non-nil, load the init script, SNIPPET.

\(fn SNIPPET BODY...)"
  (declare (indent 1) (debug t))
  (let ((sname (make-symbol "SNIPPET"))
        (began (make-symbol "BEGAN"))
        (absname (make-symbol "ABS-NAME")))
    `(let* ((,sname ,snippet)
            (,absname (if (file-name-absolute-p ,sname)
                          ,sname
                       (concat (expand-file-name ,uinit/init-directory)
                               "/" ,sname))))
       (unless (member ,absname uinit/loaded-init-files)
         (let ((pred (progn ,@body)))
           (when pred
             (condition-case err
                 (let* ((,began (current-time))
                        (result (unless uinit/debug-nop
                                  (load ,absname))))
                   (uinit/logger ,began result ,absname))
               (error (lwarn 'dot-emacs :warning
                             (format "%s: %s: %S" ,sname
                                     (car err) (cdr err)))))))))))

(defmacro uinit/require (feature &optional filename noerror)
  "If feature FEATURE is not loaded, load it from FILENAME."
  (let ((absname (make-symbol "ABSNAME"))
        (began (make-symbol "BEGAN"))
        (place (make-symbol "PLACE"))
        (result (make-symbol "RESULT")))
    `(let* ((,absname (locate-library (symbol-name ,feature)))
            (,began (current-time))
            (,place (if buffer-file-name
                        (file-name-nondirectory buffer-file-name)
                      "dot-emacs"))
            ,result)
       (if (and ,absname (member ,absname uinit/loaded-init-files))
           (setq ,result t)
         (condition-case err
             (progn (unless uinit/debug-nop
                      (require ,feature ,filename))
                    (setq ,result t))
           (error (or ,noerror
                      (lwarn ,place :warning
                             (format "%S: %s: %S" ,feature
                                     (car err) (cdr err))))))
         (uinit/logger ,began ,result (or ,absname ,feature)))
       ,result)))

(defun uinit/previous-line (&optional arg)
  (interactive "p")
  (previous-line arg)
  (beginning-of-line)
  (save-match-data
    (when (re-search-forward "^[0-9.]+ +\\[[^]]*\\] +\\(.*\\)$" nil t)
      (goto-char (match-beginning 1)))))

(defun uinit/next-line (&optional arg)
  (interactive "p")
  (next-line arg)
  (beginning-of-line)
  (save-match-data
    (when (re-search-forward "^[0-9.]+ +\\[[^]]*\\] +\\(.*\\)$" nil t)
      (goto-char (match-beginning 1)))))

(defun uinit/kill-buffer ()
  (interactive)
  (kill-buffer uinit/buffer-name))

(defun uinit/source-file-name ()
  (let (fname)
    (save-excursion
      (save-match-data
        (beginning-of-line)
        (when (re-search-forward "^[0-9.]+ +\\[[^]]*\\] +\\(.*\\)$" nil t)
          (setq fname (concat (buffer-substring-no-properties
                              (match-beginning 1)
                              (match-end 1))
                             ".el")))))
    fname))

(defun uinit/find-file ()
  (interactive)
  (let ((fname (uinit/source-file-name)))
    (when fname
      (find-file fname))))

(defun uinit/find-file-other-window ()
  (interactive)
  (let ((fname (uinit/source-file-name)))
    (when fname
      (find-file-other-window fname))))


(defun uinit/get-buffer ()
  (let ((buf (get-buffer uinit/buffer-name)))
    (or buf
        (with-current-buffer (get-buffer-create uinit/buffer-name)
          (init-report-mode)
          (current-buffer)))))

(defun uinit/logger (started success &optional init-file)
  "leave a log message in the buffer named `uinit/buffer-name'.

STARTED is the time when the loading INIT-FILE started, SUCCESS should
be non-nil if loading was successful, and INIT-FILE is the file name of the
init script."
  (with-current-buffer (uinit/get-buffer)
    (let ((inhibit-read-only t)
          (status (if success "okay" "fail"))
          (latency (float-time (time-subtract (current-time) started))))
      (goto-char (point-max))
      (insert (format "%3.5f  [%s] %s\n" latency status (or init-file "")))
      (when init-file
        (add-to-list 'uinit/loaded-init-files init-file)))))


(defun uinit/summarize ()
  (with-current-buffer (uinit/get-buffer)
    (let ((inhibit-read-only t))
      (sort-numeric-fields 1 (point-min) (point-max))
      (goto-char (point-max))
      (insert (format "--\n%3.5f  TOTAL\n"
                      (float-time (time-subtract (current-time)
                                                 uinit/time-started)))))))

(define-derived-mode init-report-mode fundamental-mode "report"
  "docstring"
  (setq snippets-font-lock-keywords '(("^\\([0-9.]+\\) +\\(\\[[^]]*\\]\\) +\\(.*\\)$"
                                       (1 font-lock-variable-name-face)
                                       (2 font-lock-comment-face)
                                       (3 font-lock-constant-face))
                                      ("^\\([0-9.]+\\) +\\([A-Z]+\\)$"
                                       (1 font-lock-warning-face)
                                       (2 font-lock-doc-face))))
  (make-local-variable 'font-lock-keywords)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'line-move-visual)
  (setq line-move-visual nil)
  (setq font-lock-keywords snippets-font-lock-keywords)
  (setq font-lock-defaults '((snippets-font-lock-keywords) t nil nil))

  (setq buffer-read-only t)
  (define-key init-report-mode-map [(return)] 'uinit/find-file)
  (define-key init-report-mode-map [(meta return)] 'uinit/find-file-other-window)
  (define-key init-report-mode-map [(o)] 'uinit/find-file-other-window)
  (define-key init-report-mode-map [(p)] 'uinit/previous-line)
  (define-key init-report-mode-map [(n)] 'uinit/next-line)
  (define-key init-report-mode-map [(q)] 'uinit/kill-buffer)
  )

(when nil
(with-current-buffer (get-buffer "*snippets*")
  (font-lock-mode 0)
  (setq snippets-font-lock-keywords '(("^\\([0-9.]+\\) +\\(\\[[^]]*\\]\\) +\\(.*\\)*$"
                                       (1 font-lock-variable-name-face)
                                       (2 font-lock-comment-face)
                                       (3 font-lock-constant-face))
                                      ("^\\([0-9.]+\\) +\\([A-Z]+\\)$"
                                       (1 font-lock-warning-face)
                                       (2 font-lock-doc-face))))
  (setq font-lock-keywords snippets-font-lock-keywords)
  (setq font-lock-defaults '((snippets-font-lock-keywords) t nil nil))
  (make-local-variable 'font-lock-keywords)
  (make-local-variable 'font-lock-defaults)
  (font-lock-mode 1))
)

(let ((do-compile (getenv "EMACS_COMPILE"))
      (nop (getenv "EMACS_NOP")))
  (if (or (null do-compile)
          (string-equal do-compile ""))
      (setq do-compile uinit/use-byte-compile)
    (setq do-compile (not (eq (string-to-number do-compile) 0))))

  (unless (null nop)
    (setq uinit/debug-nop t))

  (when do-compile
    (dolist (dir uinit/byte-compile-directories)
      (let ((d (expand-file-name dir)))
        (when (and (stringp d)
                   (file-directory-p d))
          (byte-recompile-directory d 0))))))

(provide 'uinit)
