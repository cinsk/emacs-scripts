;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;
;;; Dired and dired-x setting
;;;

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
(defun cinsk/dired-dot-dirs-first ()
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

(add-hook 'dired-after-readin-hook #'cinsk/dired-dot-dirs-first)

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


(defun cinsk/buffer-directory (&optional buffer)
  "Return the expanded directory string of the BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (expand-file-name
     (if buffer-file-name
         (file-name-directory buffer-file-name)
       default-directory))))

(defun cinsk/dired-buffers (&optional buffer)
  "Return a list of dired buffers that contains the directory of BUFFER."
  (let ((dir (cinsk/buffer-directory (or buffer (current-buffer)))))
    (remove nil
            (mapcar (lambda (buf)
                      (let ((d (cinsk/buffer-directory buf)))
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

(defun cinsk/dired-jump (orig-fun &rest args)
  "Advice for dired-jump to reuse the existing `dired' buffer."
  (let ((dir (cinsk/buffer-directory))
        (buf (car (cinsk/dired-buffers)))
        (file buffer-file-name))
    (if (null buf)
        (apply orig-fun args)
      (switch-to-buffer buf)
      (dired-revert)
      (dired-goto-file (or file dir)))))
;;(advice-add 'dired-jump :around #'cinsk/dired-jump)


;; Search filenames (e.g. C-s or C-M-s) only when the point is in the
;; file names.
(setq dired-isearch-filenames 'dwim)
