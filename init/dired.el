;; -*-emacs-lisp-*-

;;;
;;; Dired and dired-x setting
;;;

(require 'dired-x)

(when (locate-library "dired+")
  (require 'dired+))


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

(when (eq (call-process insert-directory-program nil nil nil
                        "-d" "--time-style=iso" "/") 0)
  ;; Prefer ISO time style.
  (setq dired-listing-switches (concat dired-listing-switches
                                       " --time-style=iso")))

(defvar dired-desktop-open-program
  (let ((open-sh (concat (file-name-as-directory user-emacs-directory)
                         "open.sh")))
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
(define-key dired-mode-map [(meta ?O)] 'dired-do-desktop-open)

(when (string-match "\\bgnu\\b" (symbol-name system-type))
  ;; If the operating system is gnu or gnu/linux,
  ;; we'll use GNU ls(1) --time-style option
  (setq dired-listing-switches
        (concat dired-listing-switches " --time-style=long-iso")))

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
  (interactive "p")
  (let ((buffer (get-file-buffer (dired-get-file-for-visit)))
        (frame (next-frame (selected-frame) 'visible)))
    (and (not buffer)
         (setq buffer (find-file-noselect
                       (dired-get-file-for-visit) nil nil nil)))
    (and (or (not frame)
             (eq frame (selected-frame)))
         (setq frame (make-frame)))
    (set-window-buffer (get-lru-window frame) buffer)
    (and (< arg 0)
         (select-frame-set-input-focus frame))))

(eval-after-load "dired"
  '(define-key dired-mode-map [(control return)] 'dired-find-file-other-frame))
