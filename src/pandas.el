;; -*- lexical-binding: t; -*-

(defvar pd-code-buffer " *Pandas Code*")
(defvar pd-data-buffer " *Pandas Data*")

(defvar pd-keep-temp-buffers t
  "If non-nil, do not kill temporary buffers for debugging.")

(defmacro pd-with-code-buffer (&rest body)
 "Like `with-temp-buffer`, but respects `*my-keep-temp-buffers*`."
  (declare (indent 0) (debug t))
  (let ((temp-buffer (gensym "temp-")))
    `(let ((,temp-buffer (get-buffer-create pd-code-buffer)))
       (with-current-buffer ,temp-buffer
         (erase-buffer)
         (unwind-protect
             (progn ,@body
                    (python-shell-send-buffer))
           (unless pd-keep-temp-buffers
             (kill-buffer ,temp-buffer)))))))

(defmacro pd-dolines (buffer &rest body)
 "Like `with-temp-buffer`, but respects `*my-keep-temp-buffers*`."
  (declare (indent 1) (debug t))
  `(with-current-buffer (or ,buffer (current-buffer))
     (save-excursion
       (goto-char (point-min))
       (while (not (eobp))
         (beginning-of-line)
         ,@body
         (forward-line 1)))))

(defun pd--map-nested-strings (func list)
  "Traverse LIST recursively and apply FUNC to any string found."
  (mapcar (lambda (element)
            (cond
             ((stringp element) (funcall func element)) ; Convert string
             ((listp element) (pd--map-nested-strings func element)) ; Recurse into sublist
             (t element))) ; Leave other data types untouched
          list))

(defun pd--load-dataframe (data)
  (let (cds)
    (pd-init)
    (dolist (ln data)
      (setq cds (cons (format "_PD.Series([%s])"
                              (string-join (mapcar (lambda (f) (prin1-to-string f)) ln) ","))
                      cds)))

    (pd-with-code-buffer
      (insert (format "_PD.DataFrame([%s])" (string-join (nreverse cds) ",\n  "))))))


(defun pd-dataframe-from-orgtable (&optional arg)
  (interactive "P")
  (let ((data (pd--map-nested-strings #'substring-no-properties (org-table-to-lisp))))
    (setq data (seq-remove (lambda (elm) (eq elm 'hline)) data))
    (pd--load-dataframe data)))


(defun pd-yank ()
  (let ((buf (get-buffer-create pd-data-buffer)))
    (with-current-buffer buf
      (erase-buffer)
      (clipboard-yank))
    buf))

(defun pd-dataframe-from-clipboard (&optional arg)
  "Read tabular data and return as Pandas Dataframe.

The DataFrame is evaulated from the interpreter, and can be accessible
via `_' which contains last evaulated expression."
  (interactive "P")
  (let ((buf (pd-yank))
        (ncol (if arg (prefix-numeric-value arg) nil))
        cds data)
    (pd-init)
    (with-current-buffer buf
      (setq data (pd--parse-tabular-buffer ncol)))

    (dolist (ln data)
      (setq cds (cons (format "_PD.Series([%s])"
                              (string-join (mapcar (lambda (f) (prin1-to-string f)) ln) ","))
                      cds)))

    (pd-with-code-buffer
      (insert (format "_PD.DataFrame([%s])" (string-join (nreverse cds) ",\n  "))))))


(defun pd--guess-delimiter-by-consistency (&optional expected-cols)
  "Guess the tabular delimiter based on line count consistency.
If EXPECTED-COLS is 1, return nil.
If EXPECTED-COLS is a number (> 1), check for a delimiter that
consistently yields (EXPECTED-COLS - 1) occurrences per line.
If EXPECTED-COLS is nil, fall back to the automatic guessing logic.
Prioritizes tab > comma > space in case of ties."
  (message "(pd--guess-delimiter-by-consistency %s) called" expected-cols)
  (if (and (numberp expected-cols) (= expected-cols 1))
      nil
    (save-excursion
      (goto-char (point-min))
      (let ((tab-counts nil)
            (comma-counts nil)
            (space-counts nil)
            (lines 0)
            (sample-limit 20))

        ;; 1. Collect counts per line for the sample
        (while (and (< lines sample-limit) (not (eobp)))
          (let ((line (string-trim (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position)))))
            (unless (string-empty-p line)
              (push (cl-count ?\t line) tab-counts)
              (push (cl-count ?, line) comma-counts)
              ;; Collapse multiple spaces to accurately reflect visual columns
              (let ((cleaned-space-line (replace-regexp-in-string " +" " " line)))
                (push (cl-count ?\s cleaned-space-line) space-counts))
              (setq lines (1+ lines))))
          (forward-line 1))

        ;; If an expected column count is provided, target (expected-cols - 1) delimiters
        (let ((target-count (and (numberp expected-cols) (- expected-cols 1))))

          (if target-count
              ;; --- User-specified column logic ---
              (cond
               ((and (cl-every (lambda (c) (= c target-count)) tab-counts) tab-counts) "\t")
               ((and (cl-every (lambda (c) (= c target-count)) comma-counts) comma-counts) ",")
               ((and (cl-every (lambda (c) (= c target-count)) space-counts) space-counts) " +")
               (t nil))

            ;; --- Original automatic guessing logic (when expected-cols is nil) ---
            (cl-flet ((is-consistent-p (counts)
                        (and counts
                             (cl-every (lambda (c) (> c 0)) counts)
                             (cl-every (lambda (c) (= c (car counts))) counts))))
              (cond
               ;; Ideal case: Exactly one character is perfectly uniform
               ((and (is-consistent-p tab-counts)
                     (not (is-consistent-p comma-counts))
                     (not (is-consistent-p space-counts))) "\t")

               ((and (is-consistent-p comma-counts)
                     (not (is-consistent-p tab-counts))
                     (not (is-consistent-p space-counts))) ",")

               ((and (is-consistent-p space-counts)
                     (not (is-consistent-p tab-counts))
                     (not (is-consistent-p comma-counts))) " +")

               ;; Tie/Priority fallback case
               ((is-consistent-p tab-counts) "\t")
               ((is-consistent-p comma-counts) ",")
               ((is-consistent-p space-counts) " +")

               ;; Ultimate fallback: variance check
               (t
                (let ((tab-unique (length (cl-remove-duplicates tab-counts)))
                      (comma-unique (length (cl-remove-duplicates comma-counts)))
                      (space-unique (length (cl-remove-duplicates space-counts))))
                  (cond
                   ((and (car tab-counts)   (> (car tab-counts) 0)   (<= tab-unique comma-unique) (<= tab-unique space-unique)) "\t")
                   ((and (car comma-counts) (> (car comma-counts) 0) (<= comma-unique space-unique)) ",")
                   ((and (car space-counts) (> (car space-counts) 0)) " +")
                   (t "\t"))))))))))))

(defun pd--parse-tabular-buffer (&optional expected-cols)
  "Parse the current buffer into a list of lists using the
consistency-guessed delimiter."
  ;; TODO: how User can enforce to specific number of columns?  For
  ;;       example, if data is a single colume, and contains multiple
  ;;       whitespaces, we don't want to mistranslate of multiple
  ;;       columes with whitespace delimeters.
  (let ((delimiter (pd--guess-delimiter-by-consistency expected-cols))
        (result nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-trim (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))))
          (unless (string-empty-p line)
            (if delimiter
                (push (split-string line delimiter) result)
              (push (cons line nil) result))))
        (forward-line 1)))
    (nreverse result)))

(defun pd-init ()
  (interactive)
  (let ((code "\
if '_PD' not in globals():
  if 'pd' in globals():
    if hasattr(pd, 'DataFrame'):
      globals()['_PD'] = pd
    else:
      import pandas
      globals()['_PD'] = pandas
  else:
    import pandas as pd
    globals()['_PD'] = pd"))
    (pd-with-code-buffer
      (insert code))))

(defun pd-set-display (&optional arg)
  "Adjust the display configuration of Pandas module.

Set pandas.option.display.width to the character width of the current window.
Also set pandas.options.display.max_columns to None.

If with a prefix arg, set mac_columns value to zero (autodetect),
"
  (interactive "P")
  (let* ((win (selected-window))
         (width (window-body-width win)))
    (pd-init)
    (pd-with-code-buffer
      (insert (format "_PD.options.display.width = %d\n" width))
      (insert (apply #'format "_PD.options.display.max_columns = %s
_PD.options.display.max_rows = %s" (if arg '("0" "0") '("None" "None")))))))


(when nil
(defun pandas-init (&optional arg)
  (interactive "P")
  (let ((hdr "
if '__EMACS_PD__' not in globals():
  if 'pd' in globals():
    if hasattr(pd, 'DataFrame'):
      globals()['__EMACS_PD__'] = pd
    else:
      import pandas
      globals()['__EMACS_PD__'] = pandas
  else:
    import pandas as pd
    globals()['__EMACS_PD__'] = pd
"))
    (python-shell-send-string hdr)))


(defun pandas-dataframe-from-clipboard (arg)
  ;; Convert column(s) in clipboard into DataFrame (e.g. Google Spreadsheet)
  (interactive "p")
  (pandas-init)
  (with-temp-buffer
    (yank)             ; In WSL, `gui-get-selection' seems not working
    (goto-char (point-min))
    (let (code)
      (while (not (eobp))
        (let* ((rawl (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (line (string-trim rawl "" "[\v\r\n]+"))
               (tkns (split-string line "[\t\n\r\v]+"))
               (ntks (length tkns)))
          (when (and (not (string-empty-p line)) (> ntks 0))

            (setq code (cons (format "__EMACS_PD__.Series([%s])"
                                     (string-join (mapcar (lambda (s) (format "'%s'" s)) tkns) ",")) code))))
        (forward-line 1))
      (python-shell-send-string (format "__EMACS_PD__.DataFrame([%s])" (string-join (nreverse code) ",")))
      (python-shell-send-string "_[_.ne('').any(axis=1)]"))))
)

(provide 'pandas)
