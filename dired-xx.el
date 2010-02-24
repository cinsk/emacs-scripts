;;;
;;; $Id$
;;; 
(require 'dired)
;(eval-when-compile (require 'cl))

(defvar dired-xx-file-type-use-file t
  "Use file(1) to determine the type of the file.")

(defun dired-xx-mark-files-if (predicate msg)
  "Mark files if PREDICATE is t for use in later commands.                      
PREDICATE is evaluated on each file, with an absolute pathname.
MSG as in `dired-mark-if'."
  (dired-mark-if (and (not (looking-at dired-re-dot))
                      (not (eolp)); empty line
                      (let ((fn (dired-get-filename nil t)))
                        (and fn (funcall predicate fn))))
                 msg))

(defun dired-xx-mark-files-if-file (regex msg)
  "Mark files if the output of file(1) command matches with a regular           
expression REGEX.
MSG as in `dired-mark-if'."
  (save-match-data
    (dired-xx-mark-files-if
     (lambda (fn)
       (not (null
             (string-match regex
                           (dired-xx-extract-file-type-string
                            (shell-command-to-string
                             (format "file -b \"%s\""
                                     (dired-get-filename))))))))
     msg)))

(defun dired-xx-mark-files-with-same-type (&optional marker-char)
  "Mark files with the same type of the current file.
A prefix argument means to unmark them instead.

Warning.  A call to this function may be really slow when "
  (interactive (list (if current-prefix-arg ?\040)))
  (let ((dired-marker-char (or marker-char dired-marker-char))
        (type (dired-xx-get-file-type)))
    (if (null type)
        (ding)
      (dired-xx-mark-files-if-file (format "^%s$" (regexp-quote type))
                                   (format "%s file" type)))
    (format "^%s$" (regexp-quote type))))

(defun dired-xx-extract-file-type-string (typestr)
  "Return the meaningful type string from the output of file(1) command.

This removes the trailing newline from TYPESTR, then take the first string
token of TYPESTR separated by `,'."
  (let ((s (substring typestr 0 (- (length typestr) 1)))
        (pos (string-match "," typestr)))
    (if pos
        (substring s 0 pos)
      s)))

(defun dired-xx-get-file-type ()
  "Return the type of the file in the cursor position"
  (if (dired-get-subdir)
      "directory"
    (condition-case nil
        (dired-xx-extract-file-type-string
         (shell-command-to-string (format "file -b %s" 
                                          (shell-quote-argument
                                           (dired-get-filename)))))
      (error nil))))

(defvar dired-xx-extension-history nil
  "History list of file name extension used in ...")

(defun dired-xx-mark-files-with-extension (extension &optional marker-char)
  "Mark files with the given file name extension."
  (interactive (list
                (read-from-minibuffer "File extension: "
                                      nil nil nil
                                      'dired-xx-extension-history)
                (if current-prefix-arg ?\040)))
  (let ((dired-marker-char (or marker-char dired-marker-char))
        (ext (if (string= extension "") 
                 nil
               (if (eq (elt extension 0) ?\.) ; remove dot in front if any
                   (substring extension 1)
                 extension))))
    (dired-xx-mark-files-if (lambda (fn) 
                              (if (string= (file-name-extension fn) ext)
                                  t))
                            (format "%s file" extension))))

(provide 'dired-xx)
