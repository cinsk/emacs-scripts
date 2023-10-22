;; -*-emacs-lisp-*-

;;;
;;; Dired and dired-x setting
;;;

(with-eval-after-load "dired"
  (require 'dired-x)

  (define-key dired-mode-map [(control return)] 'dired-find-file-other-frame)
  (define-key dired-mode-map [(meta ?O)] 'dired-do-desktop-open)
  )


(when nil
  (when (locate-library "dired+")
    (setq diredp-hide-details-initially-flag nil)
    (require 'dired+)))


;; (global-set-key [(meta ?F)] 'find-dired)

(defmacro setq-if-equal (symbol old-value new-value &optional nowarn)
  "setq-if-equal set SYMBOL to NEW-VALUE iff it has OLD-VALUE.
It compare the old value with OLD-VALUE using `equal' then
set it to NEW-VALUE if the old value matched.
If NOWARN is nil, and the old value is not matched with the
supplied one, a warning message is generated."
  `(progn
     (if (equal ,symbol ,old-value)
         (setq ,symbol ,new-value)
       (if (not ,nowarn)
           (progn (message "%s has unexpected value `%S'"
                           (symbol-name ',symbol) ,symbol)
                  ,old-value)))))


(add-hook 'dired-load-hook
          (lambda ()
            ;; Set dired-x global variables here.  For example:
            ;; (setq dired-guess-shell-gnutar "gtar")
            ;; Bind dired-x-find-file.
            (setq dired-x-hands-off-my-keys nil)
            ;; Make sure our binding preference is invoked.
            (dired-x-bind-find-file)
            ))

(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            (dired-omit-mode 1)
            ))

(setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
                            auto-mode-alist))

(defvar dired-desktop-open-program
  (let ((open-sh (path-join user-emacs-directory "open.sh")))
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

(setq-if-equal dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$"
               (concat dired-omit-files
                       ;; Omit RCS files
                       "\\|^RCS$\\|,v\\'"
                       ;; Omit CVS and Bitkeeper files
                       "\\|^CVS$\\|^BitKeeper\\'"
                       ;; Omit dot files
                       "\\|^\\..+\\'"
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

;;(define-key global-map "\C-x\C-j" 'dired-jump)
;;(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)
(defun dired-jump-other-frame ()
  "Like `dired-jump-other-window' but in other frame."
  (interactive)
  (let* ((file buffer-file-name)
         (dir (if file (file-name-directory file) default-directory)))
    (dired-other-frame dir)))
(global-set-key [(control x) ?f ?j] 'dired-jump-other-frame)

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
(advice-add 'dired-jump :around #'cinsk/dired-jump)


;; Search filenames (e.g. C-s or C-M-s) only when the point is in the
;; file names.
(setq dired-isearch-filenames 'dwim)
