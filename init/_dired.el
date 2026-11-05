;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;
;;; Dired and dired-x setting
;;;
(eval-when-compile
  (require 'dired)
  (require 'ediff))

(with-eval-after-load "dired"
  ;; helm package does not play nicely with dired-x-find-file.
  (setq dired-x-hands-off-my-keys t)
  (require 'dired-x)

  (keymap-set dired-mode-map "C-<return>" #'dired-find-file-other-frame)
  (keymap-set dired-mode-map "M-O" #'dired-do-desktop-open)
  )


(when nil
  (when (locate-library "dired+")
    (setq diredp-hide-details-initially-flag nil)
    (require 'dired+)))


;; (keymap-global-set "M-F" #'find-dired)

(defmacro setq-if-equal (symbol old-value new-value &optional nowarn)
  "setq-if-equal set SYMBOL to NEW-VALUE iff it has OLD-VALUE.
It compare the old value with OLD-VALUE using `equal' then
set it to NEW-VALUE if the old value matched.
If NOWARN is nil, and the old value is not matched with the
supplied one, a warning message is generated."
  (declare (debug t))
  (let ((old-val (gensym "old-val-"))
        (new-val (gensym "new-val-"))
        (warn-p (gensym "warn-p-")))
    `(let ((,old-val ,old-value)
           (,new-val ,new-value)
           (,warn-p ,nowarn))
       (if (equal ,symbol ,old-val)
           (setq ,symbol ,new-val)
         (unless ,warn-p
           (message "%s has unexpected value `%S'"
                    (symbol-name ',symbol) ,symbol)
           ,old-val)))))


(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            (dired-omit-mode 1)
            ))

(setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
                            auto-mode-alist))

(defvar dired-desktop-open-program
  (let ((open-sh (file-name-concat user-emacs-directory "open.sh")))
    (cond ((eq system-type 'darwin) "open")
          ((let ((tstr (symbol-name system-type)))
             (and (>= (length tstr) 3)
                  (string-equal (substring tstr 0 3) "gnu")
                  (file-executable-p open-sh)))
           open-sh)
          (t nil)))
  "Command to open a file in the user's desktop environment")

(defun dired-do-desktop-open ()
  (interactive)
  (when dired-desktop-open-program
    (save-window-excursion
      (dired-do-async-shell-command dired-desktop-open-program
                                    current-prefix-arg
                                    (dired-get-marked-files
                                     t
                                     current-prefix-arg)))))

(when nil
  ;; Modifying `dired-listing-switches' to use GNU ls(1) specific
  ;; options will prevent TRAMP working on Darwin as it uses BSD
  ;; ls(1).
  (when (string-match "\\bgnu\\b" (symbol-name system-type))
    ;; If the operating system is gnu or gnu/linux,
    ;; we'll use GNU ls(1) --time-style option
    (setq dired-listing-switches
          (concat dired-listing-switches
                  "--group-directories-first --time-style=long-iso"))))

;;
;; Keep "." and ".." at the top of each directory listing.
;;
;; ls(1) sorts them like any other entry, so names that sort before
;; "." in byte order (e.g. "#foo.el#", "-foo", "+foo") or a time sort
;; (`-t') can push them down.  This moves just those two lines back
;; to the top; the order of everything else is left to ls(1).
;;
(defun my/dired-dot-dirs-first ()
  "Move the \".\" and \"..\" lines to the top of the listing.
Runs from `dired-after-readin-hook', where the buffer is narrowed
to a single directory listing."
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        (modified (buffer-modified-p))
        top dot dotdot)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((name (dired-get-filename 'no-dir t)))
          (when (and name (not top))
            ;; first file line, right after the header lines
            (setq top (copy-marker (line-beginning-position))))
          (if (not (member name '("." "..")))
              (forward-line 1)
            (let* ((beg (line-beginning-position))
                   (end (min (point-max) (1+ (line-end-position))))
                   ;; keeps the text properties dired put on the line
                   (text (buffer-substring beg end)))
              (if (equal name ".") (setq dot text) (setq dotdot text))
              (delete-region beg end)))))
      (when (and top (or dot dotdot))
        (goto-char top)
        (insert (or dot "") (or dotdot "")))
      (when top (set-marker top nil)))
    (set-buffer-modified-p modified)))

(add-hook 'dired-after-readin-hook #'my/dired-dot-dirs-first)

(setq dired-omit-files
      (concat ""
              ;; lockfiles
              "^\\.?#"
              ;; Omit RCS files
              "\\|^RCS$\\|,v\\'"
              ;; Omit CVS and Bitkeeper files
              "\\|^CVS$\\|^BitKeeper\\'"
              ;; Omit dot files (except "." and "..")
              "\\|^\\.[^.]+.*\\'"
              ;; Omit .o, .lo, .Po, .Plo, .a, .la files
              "\\|.+\\.\\(o\\|lo\\|Po\\|Plo\\|a\\|la\\)\\'"))

(when nil
  ;; I want to delete all files that look like ".#gbd.tex.1.22", or
  ;; ".#link.tex.1.1.1.1" in the dired mode with a single command.
  (setq-if-equal
   dired-garbage-files-regexp
   "\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\)\\'"
   (format "\\(?:%s\\|%s\\)\\'"
           "aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc" ; TeX related
           "\\`\.#.*[0-9]")))                          ; VC related

;;(keymap-set global-map "C-x C-j" #'dired-jump)
;;(keymap-set global-map "C-x 4 C-j" #'dired-jump-other-window)
(defun dired-jump-other-frame ()
  "Like `dired-jump-other-window' but in other frame."
  (interactive)
  (let* ((file buffer-file-name)
         (dir (if file (file-name-directory file) default-directory)))
    (dired-other-frame dir)))
(keymap-global-set "C-x f j" #'dired-jump-other-frame)

(defun dired-find-file-other-frame (&optional arg)
  (interactive "P")
  (let ((buffer (get-file-buffer (dired-get-file-for-visit)))
        (frame (next-frame (selected-frame) 'visible)))
    (and (not buffer)
         (setq buffer (find-file-noselect
                       (dired-get-file-for-visit) nil nil nil)))
    (and (or (not frame)
             (eq frame (selected-frame)))
         (setq frame (make-frame)))
    (set-window-buffer (get-lru-window frame) buffer)
    (when arg
         (select-frame-set-input-focus frame))))


(defun my/buffer-directory (&optional buffer)
  "Return the expanded directory string of the BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (expand-file-name
     (if buffer-file-name
         (file-name-directory buffer-file-name)
       default-directory))))

(defun my/dired-buffers (&optional buffer)
  "Return a list of dired buffers that contains the directory of BUFFER."
  (let ((dir (my/buffer-directory (or buffer (current-buffer)))))
    (remove nil
            (mapcar (lambda (buf)
                      (let ((d (my/buffer-directory buf)))
                        (with-current-buffer buf
                          (when (and (buffer-live-p buf)
                                     (memq major-mode '(dired-mode sr-mode))
                                     (not (eq ?\s
                                              (elt (buffer-name buf) 0))))
                            (and (or (when (string-equal dir d)
                                       (message "dir: %S" d))
                                     (with-current-buffer buf
                                       (cl-member-if (lambda (d)
                                                    (string-equal dir (car d)))
                                                  dired-subdir-alist)))
                                 buf)))))
                    (buffer-list)))))

(defun my/dired-jump (orig-fun &rest args)
  "Advice for dired-jump to reuse the existing `dired' buffer."
  (let ((dir (my/buffer-directory))
        (buf (car (my/dired-buffers)))
        (file buffer-file-name))
    (if (null buf)
        (apply orig-fun args)
      (switch-to-buffer buf)
      (dired-revert)
      (dired-goto-file (or file dir)))))
;;(advice-add 'dired-jump :around #'my/dired-jump)



(defun my/dired--marked-paths ()
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

(defun my/dired--path-at-point ()
  "Return the absolute path of the entry at point, or nil."
  (let ((f (dired-get-filename nil t)))
    (and f (expand-file-name f))))

(defun my/dired--ediff-dirs (a b ask)
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

(defun my/dired--ediff-pair (a b ask)
  "Run ediff on A and B, choosing files or directories by type.
ASK is passed to `my/dired--ediff-dirs' for directory comparisons."
  (let ((a-dir (file-directory-p a))
        (b-dir (file-directory-p b)))
    (cond
     ((and a-dir b-dir)
      (my/dired--ediff-dirs a b ask))
     ((or a-dir b-dir)
      (user-error "Cannot compare a file with a directory: %s vs %s" a b))
     (t
      (ediff-files a b)))))

(defun my/dired-ediff (&optional prefix)
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
  (let ((marks (my/dired--marked-paths))
        (here (my/dired--path-at-point))
        (ask (and prefix t)))
    (cond
     ;; Case 1: exactly two marked entries.
     ((= (length marks) 2)
      (my/dired--ediff-pair (nth 0 marks) (nth 1 marks) ask))
     ;; Case 2: one marked entry, different from the entry at point.
     ((and (= (length marks) 1)
           here
           (not (string= (car marks) here)))
      (my/dired--ediff-pair (car marks) here ask))
     ;; Case 3: point is on a directory.
     ((and here (file-directory-p here))
      (if ask
          ;; Prompt for both directories ourselves so the filter and
          ;; recursive answers can be applied.
          (my/dired--ediff-dirs
           (read-directory-name "Directory A: " (file-name-as-directory here)
                                nil t)
           (read-directory-name "Directory B: " nil nil t)
           t)
        (call-interactively #'ediff-directories)))
     ;; Case 4: default file behavior.
     (t
      (call-interactively #'ediff-files)))))

(keymap-set dired-mode-map "E" #'my/dired-ediff)


;; Search filenames (e.g. C-s or C-M-s) only when the point is in the
;; file names.
(setq dired-isearch-filenames 'dwim)
