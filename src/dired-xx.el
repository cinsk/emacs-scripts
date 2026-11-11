;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;
;;; $Id$
;;;
(require 'dired)
(require 'transient)
(require 'seq)

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
  "Mark files if the output of file(1) command matches with a
regular expression REGEX.  MSG as in `dired-mark-if'."
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


;; The reason why `dired-xx-ediff' developed:
;;
;; `ediff-files' has a difficulty to extract filenames from dired.
;; When dired buffer contains files from a subdirectory, ediff assumes
;; marked files are in the current directory, not from a subdirectory.
;; `dired-xx-ediff' properly handles by grabbing the absolute pathname
;; of the marked files, pass to ediff.

(defun dired-xx--marked-paths ()
  "Return absolute paths of all explicitly marked entries in this Dired buffer.
Unlike `dired-get-marked-files', this does not fall back to the entry at point."
  (save-excursion
    (goto-char (point-min))
    (let (acc)
      (while (re-search-forward (dired-marker-regexp) nil t)
        (let ((f (dired-get-filename nil t)))
          (when f
            (push (expand-file-name f) acc))))
      (nreverse acc))))

(defun dired-xx--path-at-point ()
  "Return the absolute path of the entry at point, or nil."
  (let ((f (dired-get-filename nil t)))
    (and f (expand-file-name f))))

(defun dired-xx--ediff-dirs (a b ask)
  "Run `ediff-directories' on directories A and B.
If ASK is non-nil, prompt for a filter regexp (empty means no filter).
Subdirectories appear as entries in the session buffer; press RET on
one to open a nested directory session."
  (let ((filter (if ask
                    (read-string "Filter regexp (RET for none): ")
                  ""))
        (a (file-name-as-directory a))
        (b (file-name-as-directory b)))
    (ediff-directories a b filter)))

(defun dired-xx--ediff-pair (a b ask)
  "Run ediff on A and B, choosing files or directories by type.
ASK is passed to `dired-xx--ediff-dirs' for directory comparisons."
  (let ((a-dir (file-directory-p a))
        (b-dir (file-directory-p b)))
    (cond
     ((and a-dir b-dir)
      (dired-xx--ediff-dirs a b ask))
     ((or a-dir b-dir)
      (user-error "Cannot compare a file with a directory: %s vs %s" a b))
     (t
      (ediff-files a b)))))

(defun dired-xx-ediff (&optional prefix)
  "Run ediff from a Dired buffer, using full paths for files and directories.

Works with inserted subdirectories and `find-dired' listings.

- If two entries are marked, ediff those two.
- If one entry is marked and differs from the entry at point, ediff the
  marked entry against the entry at point.
- Otherwise, fall back to plain `ediff-files', or to directory
  comparison if point is on a directory.

Two files use `ediff-files'.  Two directories use `ediff-directories'.
Mixing a file and a directory signals an error.

With a prefix argument PREFIX, when comparing directories, prompt for
a filter regexp (RET for no filter)."
  (interactive "P")
  ;; When the dired buffer contains subdirectories or from a virtual
  ;; dired, ediff-files cannot get the pathname of default files,
  ;; rather it assumed marked files are in the current directory.  To
  ;; solve this, we need to get the absolute pathname of the marked
  ;; files.
  (let ((marks (dired-xx--marked-paths))
        (here (dired-xx--path-at-point))
        (ask (and prefix t)))
    (cond
     ;; Case 1: exactly two marked entries.
     ((= (length marks) 2)
      (dired-xx--ediff-pair (nth 0 marks) (nth 1 marks) ask))
     ;; Case 2: one marked entry, different from the entry at point.
     ((and (= (length marks) 1)
           here
           (not (string= (car marks) here)))
      (dired-xx--ediff-pair (car marks) here ask))
     ;; Case 3: point is on a directory.
     ((and here (file-directory-p here))
      (if ask
          ;; Prompt for both directories ourselves so the filter and
          ;; recursive answers can be applied.
          (dired-xx--ediff-dirs
           (read-directory-name "Directory A: " (file-name-as-directory here)
                                nil t)
           (read-directory-name "Directory B: " nil nil t)
           t)
        (call-interactively #'ediff-directories)))
     ;; Case 4: default file behavior.
     (t
      (call-interactively #'ediff-files)))))


;; Introduce transient menu for ls(1) switches.
(defvar dired-xx--initial nil
  "Switches the menu should start with.")

(transient-define-argument dired-xx--sort ()
  :description "Sort by"
  :class 'transient-switches
  :key "s"
  :argument-format "--sort=%s"
  :argument-regexp "\\(--sort=\\(?:time\\|size\\|extension\\|version\\|none\\)\\)"
  :choices '("time" "size" "extension" "version" "none"))

(defun dired-xx--current-switches ()
  "Return the current Dired switches as a list of single arguments."
  (let (out)
    (dolist (s (split-string (or dired-actual-switches "")))
      (cond ((string-prefix-p "--" s) (push s out))
            ((string-prefix-p "-" s)
             (dolist (c (append (substring s 1) nil))
               (push (format "-%c" c) out)))))
    (delete "-l" (nreverse out))))

(defun dired-xx--strip-sort (args)
  "Remove all sort-related switches from ARGS."
  (seq-remove (lambda (a)
                (or (member a '("-t" "-S" "-X" "-U" "-v"))
                    (string-prefix-p "--sort=" a)))
              args))

(defun dired-xx-apply (&optional args)
  "Apply the selected ls switches to the current Dired buffer."
  (interactive (list (transient-args 'dired-xx-menu)))
  (dired-sort-other (mapconcat #'identity (cons "-l" args) " ")))

(defun dired-xx-edit-manually (&optional args)
  "Edit the ls switches in the minibuffer, starting from the menu selection."
  (interactive (list (transient-args 'dired-xx-menu)))
  (let ((switches (read-string "ls switches (must contain -l): "
                               (mapconcat #'identity (cons "-l" args) " ")
                               'dired-xx--history)))
    (dired-sort-other switches)))

(defvar dired-xx--history nil
  "Minibuffer history for manually typed ls switches.")

(transient-define-prefix dired-xx-menu ()
  "Build ls switches for Dired."
  :init-value (lambda (obj) (oset obj value dired-xx--initial))
  ["Listing"
   ("a" "all (including dotfiles)" "-a")
   ("A" "almost all" "-A")
   ("h" "human-readable sizes" "-h")
   ("F" "classify" "-F")
   ("g" "group directories first" "--group-directories-first")
   ("r" "reverse order" "-r")]
  ["Sorting (choose one)"
   (dired-xx--sort)]
  ["Apply"
   ("RET" "apply" dired-xx-apply)
   ("e" "edit manually" dired-xx-edit-manually)])

;;;###autoload
(defun dired-xx-sort-menu (arg)
  "Open the ls switches menu.
Without ARG, start with --sort=time selected (a quick \"sort by date\").
With ARG (\\[universal-argument]), start with the current switches."
  (interactive "P")
  (setq dired-xx--initial
        (if arg
            (dired-xx--current-switches)
          (cons "--sort=time"
                (dired-xx--strip-sort (dired-xx--current-switches)))))
  (call-interactively #'dired-xx-menu))


(with-eval-after-load 'dired
  (keymap-set dired-mode-map "s" #'dired-xx-sort-menu)
  (keymap-set dired-mode-map "E" #'dired-xx-ediff))

(provide 'dired-xx)
;; dired-xx.el ends here
