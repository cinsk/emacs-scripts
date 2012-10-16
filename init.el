;; -*-emacs-lisp-*-

;;;
;;; Seong-Kook Shin's .emacs initialization file.
;;;

;;;
;;; You can download the latest version of this script at cinsk.org
;;; 
;;; $ # make sure that you don't have .emacs.d/ nor .emacs file
;;; $ rm -r .emacs.d .emacs
;;; $ # Get the source file from my repository
;;; $ git clone http://www.cinsk.org/git/emacs-scripts .emacs.d
;;;

;;;
;;; emacs packages for my personal uses are placed in $HOME/.emacs.d
;;;
(setq user-emacs-directory "~/.emacs.d/")

(if (not (file-accessible-directory-p user-emacs-directory))
    (if (yes-or-no-p
         (format "create user directory(%s)? " user-emacs-directory))
        (make-directory user-emacs-directory t)))

(setq load-path (cons (expand-file-name user-emacs-directory) load-path))


;;;
;;; Mac OS customization
;;;
(defun darwin-smart-other-frame (&optional arg)
  "Switch to other frame or call `tmm-menubar`."
  (interactive "P")
  (if (display-graphic-p)
      (other-frame arg)
    (tmm-menubar arg)))

(defun set-exec-path-from-shell-path ()
  (let ((newpath (shell-command-to-string "$SHELL -l -c 'echo $PATH'")))
    (setenv "PATH" newpath)
    (setq exec-path (split-string newpath path-separator))))

(when (eq system-type 'darwin)
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta)
  ;; sets fn-delete to be right-delete
  (global-set-key [kp-delete] 'delete-char)

  (set-exec-path-from-shell-path)

  (cond
    ;; prefer GNU ls over the native one.
    ;;
    ;; GNU ls provides "--dired" for Emacs to provide special escape
    ;; sequences for certain unusual file names.
    ;;
    ;; My system (Mountain Lion, Homebrew coreutils) provides
    ;; "/usr/local/bin/gls" where the previous version provided
    ;; "/usr/local/bin/ls".
   ((executable-find "gls")
    (setq insert-directory-program "gls"))
   ((executable-find "/usr/local/bin/ls")
    (setq insert-directory-program "/usr/local/bin/ls")))


  (when nil
    ;; These configuration seems to stop working in recent version of
    ;; Emacs 24.x. 
    (set-fontset-font "fontset-standard" 'unicode 
                      (font-spec :name "Consolas"
                                 :weight 'normal
                                 :slant 'normal
                                 :size 16)); nil 'prepend)
    (set-fontset-font "fontset-standard" 'hangul
                      (font-spec :name "NanumGothicCoding"))

    (set-face-font 'default "fontset-standard"))

  (when t
    ;; These configuration seems to work in
    ;; GNU Emacs 24.1.1 (x86_64-apple-darwin, NS apple-appkit-1038.36)
    ;; of 2012-06-11 on bob.porkrind.org

    ;; default font family
    (set-face-attribute 'default nil :family "Consolas")

    ;; default font size
    ;;
    ;; WARNING: depending on the font family, some height value may
    ;; cause a broken frame display; that is, the beginning of the
    ;; buffer is not visible.
    (set-face-attribute 'default nil :height 165)

    ;;(set-fontset-font t 'unicode (font-spec :size 20.0))

    ;; You may add :size POINT in below font-spec if you want to use
    ;; specific size of Hangul font regardless of default font size
    (set-fontset-font t 'hangul
                      (font-spec :name "NanumGothicCoding")))

  (setq default-frame-alist (append default-frame-alist
                                    '((width . 80) (height . 45)
                                      (alpha . (100 . 60)))))

  (when (display-graphic-p)
    (global-set-key [(meta ?c)] 'ns-copy-including-secondary))
  (global-set-key [(meta ?`)] 'darwin-smart-other-frame)

  ;; If Emacs is not launched in Terminal, .bashrc is not executed, so
  ;; that /usr/local/bin is not added to the PATH, so that Emacs will
  ;; not find some executables in "/usr/local/bin".
  (unless (member "/usr/local/bin" exec-path)
    (add-to-list 'exec-path "/usr/local/bin"))

  (desktop-save-mode 1))

(when (eq window-system 'x)
  ;; enable clipboard
  (setq x-select-enable-clipboard t))

;;; Although it is possible to set font faces in lisp code, I prefer
;;; to use X resource configuration.
;;;
(when nil
  ;; "NanumGothic_Coding-12"
  ;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))
  ;; (set-face-font 'default "fontset-default")
  ;; (set-fontset-font "fontset-default" '(#x1100. #xffdc)
  ;;                   '("NanumGothic_Coding" . "unicode-bmp"))
  ;; (set-fontset-font "fontset-default" 'ascii
  ;;                   '("NanumGothic_Coding" . "unicode-bmp"))
  ;; (set-fontset-font "fontset-default" 'latin-iso8859-1
  ;;                   '("NanumGothic_Coding" . "unicode-bmp"))
  ;; (set-fontset-font "fontset-default" 'hangul
  ;;                   '("NanumGothic_Coding" . "unicode-bmp"))
  ;; (set-fontset-font "fontset-default" '(#xe0bc. #xf66e)
  ;;                   '("NanumGothic_Coding" . "unicode-bmp"))
  ;; (set-fontset-font "fontset-default" 'kana
  ;;                   '("NanumGothic_Coding" . "unicode-bmp"))
  ;; (set-fontset-font "fontset-default" 'han
  ;;                   '("NanumGothic_Coding" . "unicode-bmp"))
  )



;;;
;;; package
;;;
(let ((pkgdir (concat (file-name-as-directory user-emacs-directory)
                      "package")))
  ;; PKGDIR will contains the last emacs-23 compatible package.el from
  ;; https://github.com/technomancy/package.el
  (when (and (file-readable-p pkgdir)
             (= emacs-major-version 23))
    (add-to-list 'load-path pkgdir))

  (when (and (>= emacs-major-version 23)
             (locate-library "package"))
    (require 'package)
    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/"))
    ;; According to the package.el, if `package-enable-at-startup' is
    ;; t, it will load all the packages on start up.  But it didn't.
    ;; Perhaps it's a problem related to the version (currently Emacs
    ;; 23).  Thus, I'll force to load it here.
    (package-initialize)))

;; I will install packages that is not managed by packages.el in 
;; "$HOME/.emacs.d/site-lisp".
;;
;; Note that packages in this directory has higher priority than others.
(defvar user-site-lisp-directory
  (concat (file-name-as-directory user-emacs-directory) "site-lisp")
  "a directory name that contains packages which are not managed
by package.el")

(defun add-site-lisp-packages (dir)
  "Add the subdirectories of DIR into `load-path'."
  (when (file-accessible-directory-p dir)
    (dolist (elem (directory-files-and-attributes dir))
      (let* ((fname (car elem))
             (attrs (cdr elem))
             (path (expand-file-name 
                    (concat (file-name-as-directory dir)
                            fname))))
        (when (and (not (string-equal fname "."))
                   (not (string-equal fname ".."))
                   (eq t (car attrs)))
          ;; Add directories in $HOME/.emacs.d/site-lisp/ to the `load-path'
          (message "load-path: adding %s" path)
          (add-to-list 'load-path path))))))

(add-site-lisp-packages "/usr/local/share/emacs/site-lisp")
(add-site-lisp-packages user-site-lisp-directory)



;;;
;;; diff & ediff customization
;;;

;; Note that some external packages loads 'ediff by themselves, such
;; as magit and color-theme.  Since `ediff-make-wide-display-function'
;; should be set before loading `ediff, ediff customization should be
;; placed in the first place. -- cinsk
(setq ediff-make-wide-display-function 'cinsk/ediff-make-wide-display)
(require 'ediff)

(global-set-key [(meta ?D) ?f]
                (lambda (arg) (interactive "P")
                  (cinsk/select-command arg 'ediff-files 'ediff-files3)))
(global-set-key [(meta ?D) ?b]
                (lambda (arg) (interactive "P")
                  (cinsk/select-command arg 'ediff-buffers 'ediff-buffers3)))
(global-set-key [(meta ?D) ?d]
                (lambda (arg) (interactive "P")
                  (cinsk/select-command arg 'ediff-directories
                                        'ediff-directories3)))
(global-set-key [(meta ?D) ?v] 'ediff-revision)
(global-set-key [(meta ?D) ?r] 
                (lambda (arg) (interactive "P")
                  (cinsk/select-command arg 'ediff-regions-wordwise
                                        'ediff-regions-linewise)))
(global-set-key [(meta ?D) ?w] 
                (lambda (arg) (interactive "P")
                  (cinsk/select-command arg 'ediff-windows-wordwise
                                        'ediff-windows-linewise)))

(global-set-key [(meta ?D) ?p] 'ediff-patch-file)
(global-set-key [(meta ?D) ?P] 'ediff-patch-buffer)
(global-set-key [(meta ?D) ?m] 
                (lambda (arg) (interactive "P")
                  (cinsk/select-command arg 'ediff-merge-revisions
                                        'ediff-merge-revisions-with-ancestor)))
(global-set-key [(meta ?D) (meta ?D)] 'ediff-show-registry)


(defun cinsk/select-command (arg func1 func2)
  "call interactively FUNC1 if ARG is nil, otherwise call FUNC2."
  (call-interactively (if arg func2 func1)))

(defun cinsk/ediff-revision-buffer-p (buf)
  "Return non-nil if BUF is the temporary revision file from `ediff-revision'."
  (and (buffer-file-name buf)
       (string-match "\\`.*~.*~\\'" (file-name-nondirectory
                                     (buffer-file-name buf)))))

(defun cinsk/ediff-janitor ()
  "Delete ediff-related buffers if it is a VC related files."
  (let ((ediff-buffer-A (and (cinsk/ediff-revision-buffer-p ediff-buffer-A)
                             ediff-buffer-A))
        (ediff-buffer-B (and (cinsk/ediff-revision-buffer-p ediff-buffer-B)
                             ediff-buffer-B)))
    ;; TODO: What about ediff-buffer-C?
    (ediff-janitor nil nil)))

;; no effect here
;;(setq ediff-make-wide-display-function 'cinsk/ediff-make-wide-display)

(eval-after-load "ediff"
  '(progn
     (when nil
       (add-hook 'ediff-before-setup-windows-hook
                 'cinsk/ediff-widen-frame-for-vertical-setup)
       (add-hook 'ediff-suspend-hook
                 'cinsk/ediff-narrow-frame-for-vertical-setup)
       (add-hook 'ediff-quit-hook
                 'cinsk/ediff-narrow-frame-for-vertical-setup))

     ;; (add-hook 'ediff-quit-hook 'cinsk/ediff-restore-frame)
     (add-hook 'ediff-quit-hook (lambda ()
                                  (if ediff-wide-display-p
                                      (ediff-toggle-wide-display))))

     ;; Change the algorithm perhaps find a smaller set of changes.
     ;; This makes `diff' slower.
     (setq ediff-diff-options "-d")

     ;; ignore whitespaces and newlines. (can be toggled on/off via `##')
     (setq ediff-ignore-similar-regions t)
     ;; do not create new frame for the control panel
     (setq ediff-window-setup-function 'ediff-setup-windows-plain)
     ;; If nil, ask the user to kill the buffers on exit.
     ;; (setq ediff-keep-variants nil)

     ;; Delete the buffer for the revision files on `ediff-quit'.
     (add-to-list 'ediff-cleanup-hook #'cinsk/ediff-janitor)

     ;; no effect
     ;; (setq ediff-make-wide-display-function 'cinsk/ediff-make-wide-display)
     ))


(defun diff-ediff-patch2 (&optional arg)
  "Call `ediff-patch-file' on the current buffer.

With a prefix argument, ask the user of the option to the patch
command."
  (interactive "P")
  (require 'ediff-ptch)                 ; required for `ediff-patch-options'
  (let ((new-ediff-patch-options
         (if (and arg (= (prefix-numeric-value arg) 4))
             (read-from-minibuffer (format "patch options [%s]: "
                                           ediff-patch-options)
                                   ediff-patch-options nil nil nil
                                   ediff-patch-options)
           ediff-patch-options)))
    (let ((ediff-patch-options new-ediff-patch-options))
      (call-interactively #'diff-ediff-patch))))

(eval-after-load "diff-mode"
  '(progn
     (define-key diff-mode-map [(control ?c) (control ?e)]
       'diff-ediff-patch2)))

(defvar cinsk/ediff-wide-display-policy 'center
  "Policy of the ediff frame resizing

Set to 'center so that `ediff-toggle-wide-display' will expand
the frame with the pivot in the center of the original frame.
'left causes `ediff-toggle-wide-display' will try to keep the
right corder of the original frame.  'right causes to resize with
the left corder unchanged.")

(defadvice ediff-toggle-wide-display (around cinsk/ad-ediff-toggle-wide-display
                                             ())
  (interactive)
  (let ((w (prefix-numeric-value current-prefix-arg))
        (min-width (cond ((window-live-p ediff-window-A)
                          (if (eq ediff-split-window-function 
                                  'split-window-vertically)
                              ;; ediff windows splitted like A/B
                              (window-width ediff-window-A)
                            ;; ediff windows splitted like A|B
                            (frame-width (window-frame ediff-window-A))))
                         ((buffer-live-p ediff-buffer-A)
                          (buffer-local-value 'fill-column
                                              ediff-buffer-A))
                         (t (max fill-column 70)))))
    (setq w (max min-width w))
    ;;(message "width: %S" w)

    (let ((cinsk/ediff-wide-window-width w))
      ad-do-it)))

(ad-activate 'ediff-toggle-wide-display)

;;; TODO: not working properly on ediff-merge session

(defun cinsk/ediff-make-wide-display ()
  "Construct an alist of parameters for the wide display.
Saves the old frame parameters in `ediff-wide-display-orig-parameters'.
The frame to be resized is kept in `ediff-wide-display-frame'.
This function modifies only the left margin and the width of the display.
It assumes that it is called from within the control buffer."
  (if (not (fboundp 'ediff-display-pixel-width))
      (error "Can't determine display width"))
  (let* ((frame-A (window-frame ediff-window-A))
	 (frame-A-params (frame-parameters frame-A))
         (fw (frame-width frame-A))
         (fpw (frame-pixel-width frame-A))
	 (cw (ediff-frame-char-width frame-A))
         (febw cw)                      ; frame external border width
         (fibw (- fpw (* fw cw)))       ; frame internal border width
         desired-fw desired-fpw desired-left)

    (setq ediff-wide-display-orig-parameters
	  (list (cons 'left (max 0 (eval (cdr (assoc 'left frame-A-params)))))
		(cons 'width (cdr (assoc 'width frame-A-params))))
	  ediff-wide-display-frame frame-A)

    ;;(message "wide window width: %S" cinsk/ediff-wide-window-width)
    ;;(message "split function: %S" ediff-split-window-function)
    (setq desired-fw (* cinsk/ediff-wide-window-width
                        (if (and (boundp 'ediff-3way-job) ediff-3way-job)
                            3 2)))

    ;; ensure that DESIRED-FW is smaller than the screen size
    (if (> (+ (* desired-fw cw) febw fibw) (ediff-display-pixel-width))
        (setq desired-fw (/ (- (ediff-display-pixel-width) fibw febw) cw)))

    ;;(setq desired-fpw (+ (* desired-fw cw) fbw))
    (setq desired-fpw (* desired-fw cw))
    (let ((left (eval (cdr (assoc 'left frame-A-params)))))
      (cond ((eq cinsk/ediff-wide-display-policy 'left)
             (setq desired-left (- left (* (- desired-fw fw) cw))))

            ((eq cinsk/ediff-wide-display-policy 'right)
             (setq desired-left left))

            (t                          ; center
             (setq desired-left (- left (/ (* (- desired-fw fw) cw) 2)))))

      ;; ensure that the frame will be inside of the display border.
      (if (< (- desired-left (/ febw 2)) 0)
          (setq desired-left (/ febw 2)))

      (if (> (+ desired-left (+ (* desired-fw cw) fibw (/ febw 2)))
             (ediff-display-pixel-width))
          (setq desired-left (- (ediff-display-pixel-width) 
                                (+ (* desired-fw cw) fibw (/ febw 2))))))

    ;; (message "resizing WIDTH to %S where LEFT to %S" desired-fw desired-left)

    (modify-frame-parameters
     frame-A `((left . ,desired-left) (width . ,desired-fw)
               (user-position . t)))))


(defun frame-position-for-resizing (width height &optional frame display)
  "Return the good frame position (LEFT TOP WIDTH HEIGHT) to satisfy the
new frame size WIDTH and HEIGHT regarding to the current display"
  (or width (setq width (frame-width frame)))
  (or height (setq height (frame-height frame)))
  (let* ((left (frame-parameter frame 'left))
         (top  (frame-parameter frame 'top))
         (margin (frame-margin frame))
         ;; width and height of current frame in characters.
         (cur-width (frame-width frame))
         (cur-height (frame-height frame))
         ;; width and height of the display in pixels.
         (disp-width (display-pixel-width display))
         (disp-height (display-pixel-height display))
         ;; width and height of the character in pixels.
         (char-width (frame-char-width frame))
         (char-height (frame-char-height frame))
         ;; width and height of the new frame size in pixels.
         (pwidth (+ (* char-width width) (car margin)))
         (pheight (+ (* char-height height) (cdr margin))))
    (if (> pwidth disp-width)
        ;; WIDTH is too large for the display.
        (setq width (/ (- disp-width (car margin)) char-width)
              pwidth (+ (* width char-width) (car margin))
              ;; Since `frame-margin' cannot determine the exact
              ;; margin regarding to the window system, it's better to
              ;; set LEFT zero.
              left 0))
    (if (> pheight disp-height)
        ;; HEIGHT is too large for the display.
        (setq height (/ (- disp-height (cdr margin)) char-height)
              pheight (+ (* width char-height) (cdr margin))
              ;; Since `frame-margin' cannot determine the exact
              ;; margin regarding to the window system, it's better to
              ;; set TOP zero.
              top 0))
    (when (> (+ left pwidth) disp-width)
      ;; Current LEFT cannot satisfy WIDTH
      (setq left (- left (+ (* (- width cur-width) char-width)
                            (ceiling (/ (car margin) 2.0))))
            left (if (< left 0) 0 left)))
    (when (> (+ top pheight) disp-height)
      ;; Current LEFT cannot satisfy HEIGHT
      (setq top (- top (+ (* (- height cur-height) char-height)
                          (ceiling (/ (cdr margin) 2.0))))
            top (if (< top 0) 0 top)))
    (list left top width height)))

(defun frame-max-available-width (&optional frame)
  "Return the maximum value for the possible frame width regards
to the display width"
  (let ((width (frame-width frame))
        (char-width (frame-char-width frame))
        (pwidth (frame-pixel-width frame)))
    (- (/ (- (display-pixel-width) (- pwidth (* width char-width)))
          char-width)
       ;; For safety, subtract 2 from the max-width because we don't
       ;; know the exact margin.
       ;;
       ;; TODO: Using zero in MacOS X seems to be fine.  Check in other system.
       0)))

(defun macos-make-sub-frame ()
  (interactive)
  (let ((frame (make-frame)))
    (scale-default-font-height 0.9 frame)
    (set-frame-parameter frame 'height 54)
    (set-frame-parameter frame 'left '(- 0))
    (set-frame-parameter frame 'top '(- 5000))))
    


;;;
;;; Due to my preference, I configure fonts of Emacs using X
;;; resources.  If you are not sure, insert following configuration in
;;; your $HOME/.Xdefaults-hostname where hostname is the name of the
;;; host, or the file specified in $XENVIRONMENT.  See X(7) for more.
;;;
;;; Emacs.Fontset-0:-*-DejaVu Sans Mono-*-*-*-*-14-*-*-*-*-*-fontset-dejavu,\
;;;           latin:-*-DejaVu Sans Mono-*-*-*-*-14-*-*-*-*-*-*-*, \
;;;          hangul:-*-NanumGothic_Coding-*-*-*-*-*-*-*-*-*-*-*-*
;;;
;;; Emacs*Fontset-2:-*-Consolas-*-*-*-*-14-*-*-*-*-*-fontset-consolas,\
;;;           latin:-*-Consolas-*-*-*-*-14-*-*-*-*-*-*,\
;;;         hangul:-*-NanumGothic_Coding-*-*-*-*-*-*-*-*-*-*-*
;;;
;;; Emacs.Font: fontset-dejavu
;;;

(defun xftp (&optional frame)
  "Return t if FRAME support XFT font backend."
  (let ((xft-supported))
    (mapc (lambda (x) (if (eq x 'xft) (setq xft-supported t)))
          (frame-parameter frame 'font-backend))
    xft-supported))

(defun scale-default-font-height (factor &optional frame)
  "Scale the height of the default face
New height will be calculated by (* FACTOR old-face-height)"
  (let ((height (face-attribute 'default :height)))
    (set-face-attribute 'default frame :height (round (* height factor)))))


(when (display-graphic-p)
  ;; When Emacs uses graphic display,"control + mouse wheel up"
  ;; increases the default font size whereas "control + mouse wheel
  ;; down " decreases the size.

  ;; Note that if you call `mwheel-install' after this configuration,
  ;; both [C-mouse-4] and [C-mouse-5] bindings are cleared.
  ;;
  ;; It seems that mwheel package is automatically loaded in Emacs 22
  ;; or higher.  Thus, I do not need to call `mwheel-install' any longer.

  ;; In darwin, the wheel-up and wheel-down events are [C-wheel-up]
  ;; and [C-wheel-down] respectively.
  (let ((incr [C-mouse-4]) (decr [C-mouse-5]))
    (and (eq system-type 'darwin)
         (setq incr [C-wheel-up]
               decr [C-wheel-down]))

    (global-set-key incr (lambda ()
                           (interactive)
                           (scale-default-font-height 1.1
                                                      (selected-frame))
                           (message "New face height: %d" 
                                    (face-attribute 'default :height))))
    (global-set-key decr (lambda ()
                           (interactive)
                           (scale-default-font-height 0.9
                                                      (selected-frame))
                           (message "New face height: %d" 
                                    (face-attribute 'default :height))))))

;;; Sometimes, Emacs asks for the confirmation of a command such as
;;; killing a buffer.  In that case, user should type "yes" or "no"
;;; directly.
;;; 
;;; Below configuration let the user uses "y" or "n" instead of using
;;; longer version.
(defalias 'yes-or-no-p 'y-or-n-p)

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

(defun move-key (keymap old-key new-key)
  "Move the key definition from OLD-KEY to NEW-KEY in KEYMAP."
  (let ((def (lookup-key keymap old-key))
        (alt (lookup-key keymap new-key)))
    (define-key keymap new-key def)
    (define-key keymap old-key nil)
    alt))


;;; Helpers for TAGS manipulation
(setq tags-add-tables 't)               ; do not ask to add new tags table.

(defun safe-visit-tags-table (file &optional local)
  "Call `visit-tags-table' iff FILE is readable"
  (and (file-readable-p file)
       (visit-tags-table file local)))


;;; Set up the keyboard so the delete key on both the regular keyboard
;;; and the keypad delete the character under the cursor and to the right
;;; under X, instead of the default, backspace behavior.
;;;
;; (global-set-key [delete] 'delete-char)
;; (global-set-key [kp-delete] 'delete-char)


;;;
;;; This sets up the coding mode for linux kernel sources.
;;; (originally obtained from Documentation/CodingStyle in Linux kernel tree)
;;;
(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (if (>= emacs-version 22)
      (c-set-style "linux")   ; After version ??, we have "linux" mode!
    (progn
      (c-set-style "K&R")
      (setq c-basic-offset 8))))
(add-to-list 'auto-mode-alist '("/linux.*/.*\\.[ch]$" . linux-c-mode))



;;; Emacs generates a backup file (filename plus "~") whenever a file
;;; the first time it is saved.  Uncomment below line to prevents it.
;;;
;; (setq-default make-backup-files nil)

;;;
;;; Window-less system Configuration
;;;
(when window-system
  (menu-bar-mode 1)                    ; -1 to hide, 1 to show
  (tool-bar-mode -1)                   ; -1 to hide, 1 to show
  )

;;; set input method toggle key to 'Shift-Space'
(global-set-key [?\S- ] 'toggle-input-method)

;;; From Mr. Shin's FAQ
;;; and jay's setup <http://pllab.kaist.ac.kr/~jay>
;;; General korean langauge environment setting
(require 'cl)
(when enable-multibyte-characters
  (set-language-environment "Korean")
  ;; (setq-default file-name-coding-system 'utf-8)

  ;; Default korean keyboard layout
  ;;
  ;; "" for 2 (du-bul-sik), "3" for 3 (se-bul-sik)
  (setq-default default-korean-keyboard "3")

  (setq default-input-method "korean-hangul3")

  (setq input-method-verbose-flag nil
        input-method-highlight-flag nil)

  ;;;; give highest priority to utf-8
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)

  (add-hook 'quail-inactivate-hook 'delete-quail-completions)
  (defun delete-quail-completions ()
    (when (get-buffer "*Quail Completions*")
      (kill-buffer "*Quail Completions*")))
  ;;(set-selection-coding-system 'euc-kr)
  (set-selection-coding-system 'utf-8)
  (setq x-select-request-type 'UTF8_STRING)

  ;;(unless window-system

  ;;(set-keyboard-coding-system 'nil)
  ;;(set-terminal-coding-system 'euc-kr))

  ;; Hangul Mail setting
  (setq-default sendmail-coding-system 'euc-kr)

  ;; For use of `emacs -nw' in Korean terminal emulator
  (if (and (null window-system) (null noninteractive))
      (progn
        (set-keyboard-coding-system 'utf-8)
        (set-terminal-coding-system 'utf-8)))

  ;; hangul printing for ps-mule.el
  (setq-default ps-multibyte-buffer 'non-latin-printer)

  ;; turn off C-h during input
  (eval-after-load "quail"
    '(progn
       (define-key quail-translation-keymap "\C-h"
         'quail-delete-last-char)
       ;;(define-key quail-translation-keymap "\C-?"
       ;;  'quail-translation-help)
       (define-key quail-translation-keymap "\C-?"
         'quail-delete-last-char)
       ))

  ;; The default coding system of the dired buffer is utf-8.
  (add-hook 'dired-before-readin-hook
            (lambda ()
              (set (make-local-variable 'coding-system-for-read) 'utf-8)))
  )


;;;
;;; Search and replace related configuration
;;;
(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)

(progn
  ;; The default key binding M-% and C-M-% are too difficult to type.
  ;; Since M-# and C-M-# are not used by official bindings, I'll use them.
  (global-set-key [(meta ?#)] 'query-replace)
  (global-set-key [(control meta ?#)] 'query-replace-regexp)

  ;; During isearch, M-% and C-M-% will also launching replace job
  ;; with the last search string.  Thus, I'll add M-% and C-M-% for
  ;; the consistence.
  (define-key isearch-mode-map [(meta ?#)] 'isearch-query-replace)
  (define-key isearch-mode-map [(control meta ?#)] 
    'isearch-query-replace-regexp)
    
  ;; During isearch, `M-s w' will toggle word search, which is
  ;; difficult to remember, so use `M-W' instead.
  ;;
  ;; Note that `M-w' works as original, `kill-ring-save'.
  (define-key isearch-mode-map [(meta ?W)] 'isearch-toggle-word)
  )


;;;
;;; Shell configuration
;;;

;;
;; Make the inferior shell a login shell.
;;
(setq explicit-bash-args '("--noediting" "-i" "-l"))

;; `shell' runs an inferior shell in ASCII coding system.
;; `unicode-shell' behaves the same as `shell' except it runs an inferior
;; shell in UTF-8 coding system.
(defun unicode-shell (&optional encoding)
  "Execute the shell buffer in UTF-8 encoding.
Note that you'll need to set the environment variable LANG and others 
appropriately."
  (interactive)
  (let ((coding-system-for-read (or encoding 'utf-8))
        (coding-system-for-write (or encoding 'utf-8))
        (coding-system-require-warning t))
    (call-interactively 'shell)))

;; Allow shell mode to handle color output from shell commands
;; (notably from ls --color command)
;;
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;
;; `term/shell' is similar to `shell' based on `ansi-term' code.
;;
(defun term/shell (program &optional new-buffer-name)
  "Start a terminal-emulator in a new buffer.

With a prefix argument, it prompts the user for the shell
executable.

If there is already existing buffer with the same name, switch to
that buffer, otherwise it creates new buffer.

Like `shell', it loads `~/.emacs_SHELLNAME' if exists, or
`~/.emacs.d/init_SHELLNAME.sh'.

The shell file name (sans directories) is used to make a symbol
name such as `explicit-bash-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the
shell."
  (interactive (let ((default-prog (or explicit-shell-file-name
                                       (getenv "ESHELL")
                                       shell-file-name
                                       (getenv "SHELL")
                                       "/bin/sh")))
                 (list (if (or (null default-prog)
                               current-prefix-arg)
                           (read-from-minibuffer "Run program: " default-prog)
                         default-prog))))

  ;; Pick the name of the new buffer.
  (setq term-ansi-buffer-name
	(if new-buffer-name
	    new-buffer-name
	  (if term-ansi-buffer-base-name
	      (if (eq term-ansi-buffer-base-name t)
		  (file-name-nondirectory program)
		term-ansi-buffer-base-name)
	    "shell/term")))

  (setq term-ansi-buffer-name (concat "*" term-ansi-buffer-name "*"))

  ;; In order to have more than one term active at a time
  ;; I'd like to have the term names have the *term-ansi-term<?>* form,
  ;; for now they have the *term-ansi-term*<?> form but we'll see...
  (when current-prefix-arg
    (setq term-ansi-buffer-name 
          (generate-new-buffer-name term-ansi-buffer-name)))

  (let* ((name (file-name-nondirectory program))
         (startfile (concat "~/.emacs_" name))
         (xargs-name (intern-soft (concat "explicit-" name "-args"))))
    (unless (file-exists-p startfile)
      (setq startfile (concat user-emacs-directory "init_" name ".sh")))

    (setq term-ansi-buffer-name
          (apply 'term-ansi-make-term term-ansi-buffer-name program
                 (if (file-exists-p startfile) startfile)
                 (if (and xargs-name (boundp xargs-name))
                     ;; `term' does need readline support.
                     (remove "--noediting" (symbol-value xargs-name))
                   '("-i")))))

  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-line-mode)

  ;; I wanna have find-file on C-x C-f -mm
  ;; your mileage may definitely vary, maybe it's better to put this in your
  ;; .emacs ...

  (term-set-escape-char ?\C-x)

  (switch-to-buffer term-ansi-buffer-name))

(global-set-key "\C-cd" 'shell)




;;;
;;; Buffer Menu
;;;

;; Sort by the 2nd column (buffer name) in Buffer list
(setq Buffer-menu-sort-column 2)

;; ibuffer - advanced buffer menu
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(setq ibuffer-saved-filter-groups
      '(("default"
         ("dired" (mode . dired-mode))
         ("manual" (or
                    (name . "^\\*info.*\\*$")
                    (name . "^\\*Man.*\\*$")
                    (name . "^\\*Help.*\\*$")))
         ("gnus" (or
                  (name . "\\`\\*Group\\*\\'")
                  (name . "\\`\\*Server\\*\\'")
                  (name . "\\`\\*Article .*\\*\\'")
                  (name . "\\`\\*Summary .*\\*\\'")))
         ("elisp" (or
                   (mode . emacs-lisp-mode)
                   (name . "\\`\\*scratch\\*\\'")))
         ("internal" (or
                      (name . "^TAGS$")
                      (name . "^\\*.*\\*$"))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))


;;; When a user paste clipboard content in Emacs using mouse button 2,
;;; the content will be pasted in the place at mouse click.  Comment
;;; below line for the default behavior (at mouse click).
(setq mouse-yank-at-point t)


;;; Set the default value for the title bar of the Emacs frame.  
;;;
;;; The possible format specifiers (e.g. %F or %b) are explained in
;;; the documentation of `mode-line-format'.

(setq frame-title-format
      (if (= (user-uid) 0)
          ;; If Emacs running as root, print "ROOT" just in case
          "%F - ROOT - %b"
        "%F - %b"))

(setq icon-title-format
      (if (= (user-uid) 0)
          "%F - ROOT"
        "%F"))


;;;
;;; If you are intended BS (backspace) key to work
;;; correctly on some terminals, uncomment one of below s-exp.
;;;                                                 -- cinsk
;;(global-set-key [C-?] 'backward-delete-char)
;;(global-set-key [C-h] 'backward-delete-char)


(global-set-key "\C-cc" 'compile)
(global-set-key [(control ?c) (control ?c)] 'comment-region)

(global-set-key [?\C-.] 'find-tag-other-window) ; C-x o 


(global-set-key [(control c) ?i] 'indent-region)

(global-set-key [(f11)] 'toggle-case-fold-search)

;;;
;;; ICE setup
;;;
(add-to-list 'auto-mode-alist '(".*\\.ice$" . java-mode))

;;;
;;; Abbrev mode, skeletons, and autoload settings
;;;
(when (locate-library "xskel")
  (load-library "xskel"))

(let ((abb_default "~/.abbrev_defs")
      (abb_correct (concat (file-name-as-directory user-emacs-directory)
                           "abbrev_defs")))
  ;; Prefer "~/.emacs.d/abbrev_defs" to "~/.abbrev_defs"
  (setq abbrev-file-name
        (if (file-readable-p abb_correct)
            abb_correct
          (if (file-readable-p abb_default)
              abb_default
            abb_correct))))

(require 'autoinsert)

(let ((aid_correct (concat (file-name-as-directory user-emacs-directory)
                           "insert"))
      (aid_default (if (boundp 'auto-insert-directory)
                       auto-insert-directory
                     "~/insert")))
  (setq auto-insert-directory
        (if (file-accessible-directory-p aid_correct)
            aid_correct
          aid_default)))

(add-hook 'find-file-hook 'auto-insert)

;;;
;;; cc-mode
;;;

(require 'cc-mode)

(add-hook 'c-mode-hook
          #'(lambda ()
              (safe-visit-tags-table (concat (file-name-as-directory 
                                              user-emacs-directory)
                                             "TAGS.sys") t)))

(add-hook 'c++-mode-hook
          #'(lambda ()
              (safe-visit-tags-table (concat (file-name-as-directory 
                                              user-emacs-directory)
                                             "TAGS.sys") t)))

(add-hook 'c-mode-hook (function (lambda nil (abbrev-mode 1))))
(add-hook 'c++-mode-hook (function (lambda nil (abbrev-mode 1))))

;; navigation

(when (locate-library "cc-subword")
  (require 'cc-subword)
  (define-key c-mode-base-map [(meta ?F)] 'c-forward-subword)
  (define-key c-mode-base-map [(meta ?B)] 'c-backward-subword)
  (define-key c-mode-base-map [(meta ?D)] 'c-kill-subword))

(define-key c-mode-base-map [(meta ?{)] 'c-beginning-of-defun)
(define-key c-mode-base-map [(meta ?})] 'c-end-of-defun)

(define-key c-mode-base-map [(control meta ?{)] 'c-up-conditional-with-else)
(define-key c-mode-base-map [(control meta ?})] 'c-down-conditional-with-else)

;; Highlights suspicious C/C++ constructions
(add-hook 'c-mode-common-hook (lambda () (cwarn-mode 1)))

;;; Prompt for arguments to the preprocessor for `c-macro-expand'
(setq c-macro-prompt-flag t)

(add-hook 'java-mode-hook (lambda () (subword-mode 1)))


;;;
;;; Use hippie expansion for dynamic abbreviation
;;;
(when (locate-library "hippie-exp")
  (global-set-key [(meta ?/)] 'hippie-expand))


;;;
;;; Switching between buffers using iswitchb
;;;
(iswitchb-mode 1)			; smart buffer switching mode
(setq iswitchb-default-method 'maybe-frame) ; ask to use another frame.

;;
;; Sometimes, I found that minibuffers are in the buffer list which is
;; very annoying.  I don't know why.  It should not be in the list
;; since minibuffer name is like " *Minibuf-1*" which is matched by
;; `iswitchb-buffer-ignore'.  Need to find the cause. -- cinsk
;;
(setq iswitchb-buffer-ignore '("\\*Minibuf[^*]*\\*"))


;;;

;;;
;;; Minor Mode configuration
;;;

;; imenu mode
;;(add-hook 'c-mode-hook (function (lambda nil (imenu-add-to-menubar))))
;;(add-hook 'c++-mode-hook (function (lambda nil (imenu-add-to-menubar))))

;;(add-hook 'c-mode-hook (function (lambda nil (which-function-mode))))
;;(add-hook 'c++-mode-hook (function (lambda nil (which-function-mode))))

;;(when window-system
;;  (which-function-mode 1))          ; display function names in mode-line

(global-font-lock-mode 1)           ; every buffer uses font-lock-mode
(line-number-mode 1)                ; show line number in mode-line
(column-number-mode 1)              ; show column number in mode-line

(setq resize-minibuffer-mode t)		; ensure all contents of mini
					; buffer visible

(ffap-bindings)                         ; context-sensitive find-file

;;;
;;; TAB & space setting
;;;
(setq-default indent-tabs-mode nil)	; do not insert tab character.

(defun source-untabify ()
  "Stealed from Jamie Zawinski's homepage,
http://www.jwz.org/doc/tabs-vs-spaces.html
Remove any right trailing whitespaces and convert any tab
character to the spaces"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
        (untabify (1- (point)) (point-max))))
  nil)

;; These hook configuration ensures that all tab characters in C, C++
;; source files are automatically converted to spaces on saving.
(add-hook 'c-mode-hook '(lambda () 
                          (make-local-variable 'write-contents-hooks)
                          (add-hook 'write-contents-hooks 'source-untabify)))
(add-hook 'c++-mode-hook '(lambda () 
                            (make-local-variable 'write-contents-hooks)
                            (add-hook 'write-contents-hooks 'source-untabify)))


;;;
;;; hungray Delete
;;; 
(defun zap-to-nonspace ()
  "Delete all whitespace up to the next non-whitespace char."
  (interactive)
  (save-excursion
    (let ((start (point))
          (end (point-max)))
      (if (re-search-forward "[^ \n\t\v]" nil t)
          (setq end (min (1- (point)) end)))
      (kill-region start end))))

(defun delete-chars-forward-with-syntax ()
  "Delete forward all characters that have the same syntax element."
  (interactive)
  (let ((beg (point-marker))
        (chr (char-after)))
    (if (not (null chr))
        (progn
          (skip-syntax-forward (string (char-syntax (char-after))))
          (if (not (= beg (point)))
              (kill-region beg (point)))))))

(defun delete-chars-backward-with-syntax ()
  "Delete backward all characters that have the same syntax element.

NOTE: not fully implemented yet."
  (interactive)
  (let ((beg (point-marker))
        (chr (char-after)))
    (if (not (null chr))
        (progn
          (skip-syntax-backward (string (char-syntax chr)))
          ;;(message (buffer-substring-no-properties beg (point)))))))
          (message "%s %s" beg (point))
          (if (not (= beg (point)))
              (kill-region (+ beg 1) (point))
            (delete-char 1))
          (goto-char (- beg 1))))))

(global-set-key [(control ?c) (control ?d)] 'delete-chars-forward-with-syntax)



(when nil
  ;; Support for GNU global, the source code tag system
  (load-library "gtags")
  (add-hook 'c-mode-hook '(lambda () (gtags-mode 1)))
  (add-hook 'c++-mode-hook '(lambda () (gtags-mode 1))))

;;;
;;; Colors
;;;
;;(set-background-color "rgb:0000/1500/8000")
;;(set-foreground-color "white")
;;(set-cursor-color "")
;;(set-mouse-color "")
;;(set-face-foreground 'highlight "white")
;;(set-face-background 'highlight "slate blue")
;;(set-face-background 'region "slate blue")
;;(set-face-background 'secondary-selection "turquoise")

;;;
;;; emacs server
;;;
;;(server-start)

;;;
;;; I prefer case-sensitive search & replace
;;;
(setq-default case-fold-search nil)
(setq-default tags-case-fold-search nil)

(fset 'find-next-tag "\C-u\256")        ; macro for C-u M-.
(fset 'find-prev-tag "\C-u-\256")       ; macro for C-u - M-. 

(global-set-key "\M-]" 'find-next-tag)
(global-set-key "\M-[" 'find-prev-tag)

;;(global-set-key [up]   '(lambda () (interactive) (scroll-down 1)))
;;(global-set-key [down] '(lambda () (interactive) (scroll-up 1)))

(fset 'scroll-other-frame "\C-xo\C-v\C-xo")      ; C-x o C-v C-x o
(fset 'scroll-other-frame-down "\C-xo\366\C-xo") ; C-x o M-v C-x o

(global-set-key [(meta shift prior)] 'scroll-other-frame-down)
(global-set-key [(meta shift next)] 'scroll-other-frame)



(global-set-key [(control meta ?\])] #'forward-page)
(global-set-key [(control meta ?\[)] #'backward-page)


;;;
;;; Window/Frame configuration
;;;
(defun toggle-current-window-dedication ()
  "Toggle current window as dedicated"
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))
(global-set-key [Scroll_Lock] 'toggle-current-window-dedication)

(defun frame-max-available-width (&optional frame)
  "Return the maximum value for the possible frame width regards
to the display width"
  (let ((width (frame-width frame))
        (char-width (frame-char-width frame))
        (pwidth (frame-pixel-width frame)))
    (- (/ (- (display-pixel-width) (- pwidth (* width char-width)))
          char-width) 2)))

(defun current-frame-configuration-only (&optional frame)
  "Return a list describing the positions and states of FRAME only.

This function behaves similar to `current-frame-configuration'
except the return value contains the information of specified
FRAME only."
  (if (null frame)
      (setq frame (window-frame (selected-window))))
  (let ((fc nil))
    (mapc (lambda (f)
            (if (eq (car f) frame)
                (setq fc f)))
          (cdr (current-frame-configuration)))
    (list 'frame-configuration fc)))


(defun set-this-frame-configuration (configuration)
  "Restore the frame to the state described by CONFIGURATION.

This function behaves similar to `set-frame-configuration' except
it will not affect the other frames that are not described in
CONFIGURATION."
  (let ((old-func (symbol-function 'iconify-frame)))
    (fset 'iconify-frame (lambda (&optional frame) nil))
    (set-frame-configuration configuration t)
    (fset 'iconify-frame old-func)
    nil))


(defun reverse-other-window (arg) 
  "Reverse `other-window' with no argument"
  (interactive "p")
  (other-window (- arg)))

(defun first-window ()
  "Select the first window of the current frame."
  (let ((window nil))
    (mapcar '(lambda (w)
               (let ((edges (window-edges w)))
                 (and (eql (car edges) 0)
                      (eql (cadr edges) 0)
                      (setq window w)))) (window-list))
    window))

(defun abs-other-window (index)
  "Same as \\[other-window] except the base is the first window not the
current window"
  (interactive "p")
  (select-window (first-window))
  (other-window index))

(global-set-key [(control x) ?w ?0]
                '(lambda () (interactive) (abs-other-window 0)))
(global-set-key [(control x) ?w ?1]
                '(lambda () (interactive) (abs-other-window 1)))
(global-set-key [(control x) ?w ?2]
                '(lambda () (interactive) (abs-other-window 2)))
(global-set-key [(control x) ?w ?3]
                '(lambda () (interactive) (abs-other-window 3)))
(global-set-key [(control x) ?w ?4]
                '(lambda () (interactive) (abs-other-window 4)))
(global-set-key [(control x) ?w ?5]
                '(lambda () (interactive) (abs-other-window 5)))
(global-set-key [(control x) ?w ?6]
                '(lambda () (interactive) (abs-other-window 6)))
(global-set-key [(control x) ?w ?7]
                '(lambda () (interactive) (abs-other-window 7)))
(global-set-key [(control x) ?w ?8]
                '(lambda () (interactive) (abs-other-window 8)))
(global-set-key [(control x) ?w ?9]
                '(lambda () (interactive) (abs-other-window 9)))

;;(global-set-key [C-tab] 'other-window)  ; C-x o
;;(global-set-key [S-iso-lefttab] 'reverse-other-window)
;;(global-set-key [(backtab)] 'reverse-other-window)
(global-set-key [(control tab)] 'smart-other-frame-or-window)

;; I want C-<tab> works consistently even in minibuffer-mode.
;; Since, C-<tab> is bound to `file-cache-minibuffer-complete'
;; in the minibuffer mode, I'll replace to S-<tab>.
(define-key minibuffer-local-map [(backtab)] 'file-cache-minibuffer-complete)
(define-key minibuffer-local-map [(control tab)] 'smart-other-frame-or-window)

(global-set-key [(control x) ?w ?n] 'other-window)
(global-set-key [(control x) ?w ?o] 'other-window)
(global-set-key [(control x) ?w ?p] 'reverse-other-window)
(global-set-key [(control x) ?w ?k] 'delete-window)
(global-set-key [(control x) ?w ?K] 'delete-other-window)

(defun first-window ()
  "Return the first window of the current frame"
  (labels ((distance (win)
                     (let ((edges (window-edges win)))
                       (+ (car edges) (cadr edges)))))
    (let (cand-win (cand-dist 99999))
      (dolist (win (window-list) cand-win)
        (let ((dist (distance win)))
          (if (< dist cand-dist)
              (setq cand-win win
                    cand-dist dist)))))))

(defun nth-window (n)
  "Select Nth window in cyclic ordering of windows."
  (interactive "P")
  (labels ((position (item seq)
                     (let ((count 0))
                       (catch 'found
                         (dolist (v seq nil)
                           (if (eq v item)
                               (throw 'found count)
                             (setq count (1+ count))))))))
    (let ((pos (if n (prefix-numeric-value n)
                 0)))
      ;; (message "position: %s" pos))
      (other-window (+ pos (let ((winlist (window-list)))
                             ;;(nconc winlist winlist)
                             (position (first-window) 
                                       winlist)))))))

(defun smart-other-window ()
  "This calls `other-window' if there are more than one window, otherwise
calls `iswitchb'"
  (interactive)
  (if (one-window-p t 1)
      (call-interactively 'iswitchb-buffer)
    (call-interactively 'other-window)))

(defun smart-other-frame (arg)
  "This calls `other-frame' if there are more than one frame, otherwise calls
`other-window'"
  (interactive "p")
  (if (> (length (frame-list)) 1)
      (other-frame arg)
    (other-window arg)))

(defun smart-other-frame-or-window (&optional arg)
  "Switch focus to other window or frame."
  (interactive "p")
  (if (one-window-p 'nomini)
      (if (> (length (frame-list)) 1)
          (other-frame arg)
        (call-interactively 'iswitchb-buffer))
    (other-window arg)))

(defun reverse-smart-other-frame (arg)
  "This calls `other-frame' if there are more than one frame, otherwise calls
`other-window'"
  (interactive "p")
  (if (> (length (frame-list)) 1)
      (other-frame (- arg))
    (other-window (- arg))))

(global-set-key [(control x) ?o] 'smart-other-frame)
(global-unset-key [(control x) ?f])
(global-set-key [(control x) ?f ?f] 'new-frame)
(global-set-key [(control x) ?f ?k] 'delete-frame)
(global-set-key [(control x) ?f ?K] 'delete-other-frames)
(global-set-key [(control x) ?f ?n] 'smart-other-frame)
(global-set-key [(control x) ?f ?o] 'smart-other-frame)
(global-set-key [(control x) ?f ?p] 'reverse-smart-other-frame)

(defun run-command-other-frame (command)
  "Run COMMAND in a new frame."
  (interactive "CC-x 5 M-x ")
  (select-frame (new-frame))
  (call-interactively command))
(global-set-key "\C-x5\M-x" 'run-command-other-frame)


;;;
;;; Markdown mode
;;;
(when (locate-library "markdown-mode")
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist
               '("\\.md" . markdown-mode)))


;;;
;;; vim-modeline
;;;
(let ((vim-modeline-path (expand-file-name
                          (concat (file-name-as-directory user-emacs-directory)
                                  "vim-modeline"))))
  (when (file-accessible-directory-p vim-modeline-path)
    (add-to-list 'load-path vim-modeline-path)
    (when (locate-library "vim-modeline")
      (require 'vim-modeline)
      (add-hook 'find-file-hook 'vim-modeline/do))))


;;;
;;; CVS
;;;
(defun pop-to-cvs-buffer (arg)
  "Select \"*cvs*\" buffer in some window, preferably a different one.
If the buffer is not found, call `cvs-examine' interactively.
With a prefix argument, call `cvs-examine' with the prefix argument, 16."
  (interactive "P")
  (let ((buf (get-buffer "*cvs*")))
    (if arg
        (let ((prefix-arg '(16)))       ; C-u C-u
          (call-interactively #'cvs-examine))
      (if buf
          (pop-to-buffer buf)
        (call-interactively #'cvs-examine)))))

;;(global-set-key [f2] #'pop-to-cvs-buffer)


;;;
;;; Git
;;;
(setq git-show-uptodate t
      git-show-ignored t
      git-show-unknown t)

(eval-after-load "git"
  '(progn
     (define-key git-status-mode-map [(meta ?u)] 'git-refresh-status)))

(when (locate-library "git")
  (require 'git))

(when (locate-library "magit")
  (require 'magit))

(when nil
  ;; I do not use egg anymore.
  (let ((egg-dir (concat (file-name-as-directory 
                          (expand-file-name user-emacs-directory)) "egg")))
    (if (file-accessible-directory-p egg-dir)
        (progn
          (add-to-list 'load-path egg-dir)
          (when (locate-library "egg")
            (require 'egg))))))

;;;
;;; vc-jump
;;;
(when (locate-library "vc-jump")
  (require 'vc-jump)
  ;; I prefer magit over egg, egg over git
  (add-to-list 'vc-status-assoc
               (cons 'Git 
                     (cond ((fboundp 'magit-status) #'magit-status)
                           ((fboundp 'egg-status) #'egg-status)
                           (#'git-status))))
  (global-set-key [f12] 'vc-jump))


(when (locate-library "winner")
  ;; A history manager for window configuration.
  ;; `C-c left' for undo, `C-c right' for redo
  (require 'winner)
  (winner-mode 1))

(when (locate-library "windmove")
  ;; Select a window by pressing arrow keys.
  (require 'windmove)
  ;; The default modifier is 'shift which is not suitable for me since
  ;; I use `shift-left' and `shift-right' bindings in org-mode.
  ;;
  ;; Since my keyboard (HHK) has separate Alt key (apart from Meta
  ;; key), I use the 'alt instead of 'shift -- cinsk
  (windmove-default-keybindings 'alt))



;;;
;;; Quick Frame Configuration Load/Save
;;;
(global-set-key [(control f3)] '(lambda ()
                                  "Quick frame load"
                                  (interactive)
                                  (jump-to-register ?\x3)
                                  (message "Load saved frame configuration")))

(global-set-key [(control f4)] '(lambda ()
                                  "Quick frame save"
                                  (interactive)
                                  (frame-configuration-to-register ?\x3)
                                  (message "Frame configuration saved")))


(defun import-buffer-region (&optional after-import)
  "Copy region from the current buffer to the previous buffer.

Once called, Emacs enters in recursive edit mode.  Marking a region
in some buffer then press \\[exit-recursive-edit] will copy the region
into the buffer at the invocation time.

If the function AFTER-IMPORT is non-nil, this function will call
AFTER-IMPORT with the buffer where the user press
\\[exit-recursive-edit].  In the AFTER-IMPORT, the mark is set to
the beginning of the inserted text, and the point is set to the
end of the inserted text.

This function uses `recursive-edit' internally."
  (interactive)
  (let* ((map (current-global-map))
         (old-binding (lookup-key map [(control meta ?c)])))
    (substitute-key-definition
     'exit-recursive-edit
     'exit-import-buffer-region map)
    ;; (define-key map [(control meta ?c)] 'exit-import-buffer-region)

    (let ((old-buffer (current-buffer))
          (src-buffer (unwind-protect
                          (catch 'exit-from-import
                            (message "Use `%s' when done, or use `%s' to abort."
                                     (substitute-command-keys "\\[exit-import-buffer-region]")
                                     (substitute-command-keys "\\[abort-recursive-edit]"))
                            (recursive-edit))
                        ;; (define-key map [(control meta ?c)] old-binding))))
                        (substitute-key-definition
                         'exit-import-buffer-region
                         'exit-recursive-edit
                         map))))
      (when (buffer-live-p old-buffer)
        (let ((display-buffer-reuse-frames t)
              start end)
          (pop-to-buffer old-buffer)
          (with-current-buffer src-buffer
            (when (and mark-active
                       (or (and transient-mark-mode
                                (use-region-p))
                           (not transient-mark-mode)))
              (setq start (region-beginning)
                    end (region-end))))
          (when (and start end)
            (push-mark)
            (insert-buffer-substring src-buffer start end)
            (and after-import
                 (funcall after-import src-buffer))
            (pop-mark)))))))

(defun exit-import-buffer-region ()
  (interactive)
  (throw 'exit-from-import (current-buffer)))

(defun line-numbers-on-region (begin end &optional start)
  "Insert line numbers on the region.

When called interactively, it insert the line number starting
from 1, in the region.  A numeric prefix argument specifies the
starting number."
  (interactive (list
                (region-beginning)
                (region-end)
                (if current-prefix-arg 
                    (prefix-numeric-value current-prefix-arg)
                  1)))
  (unless start (setq start 1))
  (let ((begin (region-beginning))
        (end (region-end)))
    (save-restriction
      (let* ((lines (count-lines begin end))
             (width (length (format "%d" (1- (+ lines start)))))
             (fmt (format "%%%dd: " width)))
        (goto-char begin)
        (beginning-of-line)
        (dotimes (i lines)
          (insert (format fmt (+ start i)))
          (forward-line))))))

(global-set-key [(control ?c) ?n] #'line-numbers-on-region)


;;(require 'autofit-frame)
;;(add-hook 'after-make-frame-functions 'fit-frame)
;;
;;(add-hook 'temp-buffer-show-hook
;;          'fit-frame-if-one-window 'append)


;;;
;;; Emacs Lisp Mode
;;;

(add-hook 'emacs-lisp-mode-hook 
          '(lambda ()
             (safe-visit-tags-table (concat (file-name-as-directory 
                                             user-emacs-directory)
                                            "TAGS.emacs") t)))

(eval-after-load "lisp-mode"
  '(progn
     (define-key emacs-lisp-mode-map [f5] 'eval-buffer)
     (define-key emacs-lisp-mode-map [(control c) ?\|] 'eval-region)))


;;;
;;; Common Lisp Mode -- from clisp-2.38/editors.txt
;;;
;;; It seems that Emacs already have `lisp-eval-last-sexp' that has
;;; the same feature of `

;; clisp does not work with slime package for now -- cinsk
;;(setq inferior-lisp-program "clisp -I -q -E utf-8")
;;(setq inferior-lisp-program "sbcl")

(defun lisp-macroexpand-region (start end &optional and-go)
  "Macroexpand the current region in the inferior Lisp process.
Prefix argument means switch to the Lisp buffer afterwards."
  (interactive "r\nP")
  (comint-send-string
   (inferior-lisp-proc)
   (format "(macroexpand-1 (quote %s))\n"
           (buffer-substring-no-properties start end)))
  (if and-go (switch-to-lisp t)))


(defun lisp-macroexpand-sexp (&optional and-go)
  "Macroexpand the next sexp in the inferior Lisp process.
Prefix argument means switch to the Lisp buffer afterwards."
  (interactive "P")
  (lisp-macroexpand-region (point) (scan-sexps (point) 1) and-go))

(eval-after-load "inf-lisp"
  '(define-key inferior-lisp-mode-map [(control ?x) (control ?m)] 
     'lisp-macro-expand-sexp))

(define-key lisp-mode-map [(control ?x) (control ?m)] 'lisp-macro-expand-sexp)


;;;
;;; slime
;;;
(when (locate-library "slime-autoloads")
  (eval-after-load "slime" 
    '(progn 
       (slime-setup)
       ;; C-c C-b slime-eval-buffer
       ;; C-c C-e slime-eval-last-expression
       ;; C-c C-r slime-eval-region

       ;; `M-x slime-interrupt' moved to `C-c C-B' from `C-c C-b'
       (move-key slime-mode-map [(control ?c) (control ?b)]
                 [(control ?c) (control ?B)])
       (move-key slime-mode-map [(control ?c) (control ?e)]
                 [(control meta ?\:)])
       ;; C-c v   slime-describe-symbol
       ;; C-c f   slime-describe
       ;;(define-key slime-mode-map [(control ?c) ?v]         'slime-describe-symbol)
       ;;(define-key slime-mode-map [(control ?c) ?f]         'slime-describe-function)
       (define-key slime-mode-map [(control ?c) (control ?e)]
         'slime-eval-last-expression)
       (define-key slime-mode-map [(control ?c) (control ?b)]
         'slime-eval-buffer)))
  (require 'slime-autoloads))


;; clisp does not work with slime package for now -- cinsk
;;(setq inferior-lisp-program "clisp -I -q -E utf-8")
(if (locate-file "sbcl" exec-path)
    (setq inferior-lisp-program "sbcl"))



;;;
;;; quack (enhanced support for scheme-mode)
;;;
(when (locate-library "quack")
  (require 'quack)
  (setq quack-browse-url-browser-function 'quack-w3m-browse-url-other-window)
  (setq quack-fontify-style 'emacs)
  (setq quack-default-program "mzscheme"))

(defun scheme-grep-symbols-on-region ()
  "Insert all global symbols into the selected buffer"
  (interactive)
  (let ((src (current-buffer))
        (dst (get-buffer-create "*scheme-tmp*"))
        (begin (region-beginning))
        (end (region-end)))
    (save-excursion
      (set-buffer dst)
      (erase-buffer)
      (scheme-mode)
      (insert "(provide "))
    (save-excursion
      (narrow-to-region begin end)
      (goto-char (point-min))
      (while (re-search-forward
              "(define[ \t\v\n]+(?[ \t\v\n]*\\([^ )\t\v\n]*\\)" nil t)
        (let ((word (match-string-no-properties 1)))
          (set-buffer dst)
          (insert word)
          ;;(indent-according-to-mode)
          ;;(newline)
          (newline-and-indent)
          (set-buffer src))))
    ))


;;;
;;; LaTeX mode
;;;

(defun bounds-of-word-markers (&optional no-region)
  "Return the start and end buffer locations for the word at point.

The value is a cons cell (START-MARK . END-MARK) giving the start
and end markers.  If NO-REGION is not nil and there is no word at point,
this function returns a cons cell of current region."
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (if (and (not bounds) (not no-region) mark-active)
        (setq bounds (cons (region-beginning) (region-end))))

    (if bounds
        (cons (set-marker (make-marker) (car bounds))
              (set-marker (make-marker) (cdr bounds))))))

(defun latex-enclose-word (&optional arg)
  "Enclose current word with the supplied command name

After enclosing the current word, this function set the marker at
the beginning of the word, and move the point to the end of the
word.

If a prefix argument is given, this function uses the region
instead of the current word."
  (interactive "P")
  (let ((range (bounds-of-word-markers))
        (collect nil) (default nil))
    (if (boundp 'latex-command-name-history)
        (progn
          (setq collect latex-command-name-history)
          (setq default (car latex-command-name-history))))
    (if range
        (let ((cmdname (completing-read 
                        (if default
                            (format "Command name[%s]: " default)
                          "Command name: ")
                        collect nil nil nil
                        'latex-command-name-history default)))
          (goto-char (car range))
          (insert-before-markers (format "\\%s{" cmdname))
          (goto-char (cdr range))
          (insert "}")
          (goto-char (car range))
          (push-mark)
          (goto-char (cdr range))))))

(eval-after-load "tex-mode"
  '(progn
     (define-key tex-mode-map [(control ?c) ?e] 'latex-enclose-word)))


(defun fill-text-line-paragraph (begin end)
  "Convert each line in the region to a filled-paragraph"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)

      (let ((begin (set-marker (make-marker) begin))
            (end (set-marker (make-marker) end)))
        (set-marker-insertion-type end t)
        (beginning-of-line)
        (goto-char begin)
        (while (eq (forward-line) 0)
          (newline))

        (goto-char begin)
        (while (progn
                 (fill-paragraph nil)
                 (eq (forward-line) 0)))))))

(global-set-key [(control ?c) ?q] 'fill-text-line-paragraph)


;;;
;;; MMM mode
;;;
(let ((mmm-dir (expand-file-name 
                (concat (file-name-as-directory user-emacs-directory)
                        "mmm-mode"))))
  ;; If MMM mode is installed in $HOME/.emacs.d/mmm-mode/
  (when (file-accessible-directory-p mmm-dir)
    (add-to-list 'load-path mmm-dir)
    (add-to-list 'Info-directory-list mmm-dir)))

(when (locate-library "mmm-auto")

  (eval-after-load "mmm-mode"
    ;; It seems that mmm-mode 0.4.8 will reset the mmm-related face
    ;; attributes after loading mmm-mode.el.  To prevent resetting,
    ;; set the background of the faces AFTER loading mmm-mode.el 
    '(progn
       ;; By default, mmm-mode uses faces with bright background for
       ;; the submodes.   I don't like the bright background for most faces.
       (set-face-background 'mmm-code-submode-face "black")
       (set-face-background 'mmm-declaration-submode-face "black")
       (set-face-background 'mmm-default-submode-face "black")))

  (require 'mmm-auto)

  ;; DO NOT SET `mmm-global-mode' to t!!!  If you do in mmm-mode
  ;; 0.4.8, some mysterious bugs will happen.  Particularly, on ediff
  ;; control panel will not listen to your key input on ediff-patch-*
  ;; command.
  (setq mmm-global-mode 'maybe)

  ;; `mmm-submode-decoration-level' can be 0, 1, or 2. (0: no color)
  (setq mmm-submode-decoration-level 2)

  (setq mmm-mode-ext-classes-alist 
        '((xhtml-mode nil html-js)
          (xhtml-mode nil embedded-css)
          (html-mode nil html-js)
          (html-mode nil embedded-css)
          (nxml-mode nil html-js)
          (nxml-mode nil embedded-css)
          ))

  )



;;;
;;; XML configuration
;;;
(defun lzx-nxml-mode ()
  "OpenLaszlo XML Mode"
  (interactive)
  (nxml-mode)
  (make-local-variable 'nxml-child-indent)
  (setq nxml-child-indent 4))


(eval-after-load "nxml-mode"
  '(progn
     ;; Make a slash automatically completes the end-tag
     (setq nxml-slash-auto-complete-flag t)
     (define-key nxml-mode-map [(control ?c) (control ?e)]
       'nxml-enclose-paragraph)

     ;; install abbrev table
     (add-hook 'nxml-mode-hook (function (lambda nil (abbrev-mode 1))))))

(when (locate-library "rng-auto")
  ;; For legacy nxml-mode which does not use `provide' for nxml-mode.
  (load (locate-library "rng-auto"))

  ;; For nxml-version less than or equal to "20041004" (my Gentoo), I
  ;; need to load rng-loc.el to use `rng-schema-locating-files'.
  (when (and (boundp 'nxml-version)
             (locate-library "rng-loc")
             (not (string-lessp "20041004" nxml-version)))
    (load (locate-library "rng-loc")))

  ;; `sgml-mode' adds an entry to `magic-mode-alist' so that
  ;; `auto-mode-alist' to `nxml-mode' might not work.  To work around
  ;; this, define an alias for `xml-mode' to `nxml-mode'.
  (defalias 'xml-mode 'nxml-mode)

  (setq auto-mode-alist (cons '("\\.\\(xml\\|pvm\\|rss\\)\\'" . nxml-mode)
                              auto-mode-alist)))

(when (locate-library "nxml-mode")
  ;; Emacs 24.x built-int nxml-mode provides a package to be used with
  ;; `require'.
  
  (require 'nxml-mode))

(when (fboundp 'nxml-mode)
  (setq auto-mode-alist (cons '("\\.lzx\\'" . lzx-nxml-mode)
                              auto-mode-alist))

  ;; All .html files that I(cinsk) generate are XHTML files.
  ;; Thus, I choose `nxml-mode' over other major modes.
  (add-to-list 'auto-mode-alist '("/public_html/.*\\.s?html?\\'" . nxml-mode))

  ;; Current nxml-mode package (nxml-mode-20041004-r3) in Gentoo Linux
  ;; install schema files (schemas.xml) in
  ;; "/usr/share/emacs/etc/nxml-mode/schema/", although
  ;; `rng-schema-locating-files-default' points to
  ;; "/usr/share/emacs/site-lisp/nxml-mode/schema/".
  ;;
  ;; If `rng-schema-locating-files-default' points wrong place, warn
  ;; for malfunction of nxml-mode's auto-completion (C-RET)
  (dolist (file rng-schema-locating-files-default)
    (if (string-match "^/usr/" file)
        (if (not (file-readable-p file))
            (lwarn '(dot-emacs) :warning
                   (format "cannot access default schema for nxml-mode")))))

  ;; Adding .emacs.d/schema/schemas.xml for schema searching path
  (let ((schema-file (concat (file-name-as-directory
                              (expand-file-name user-emacs-directory))
                             "schema/schemas.xml")))
    (when (and (file-readable-p schema-file)
               (not (member schema-file rng-schema-locating-files)))
      (setq rng-schema-locating-files-default
            (delete "schemas.xml" rng-schema-locating-files))
      (add-to-list 'rng-schema-locating-files schema-file)
      (add-to-list 'rng-schema-locating-files "schemas.xml"))))


                   
      
      





(defun nxml-enclose-paragraph (start end prefix)
  "Enclose each paragraph with the element in the region.
By default, <para> element is used.  A prefix argument will give you a
chance to change the name of the element."
  (interactive "*r\nP")
  (let (curpos 
        (done nil) (elname "para"))
    (if (not (eq (prefix-numeric-value prefix) 1))
        (setq elname (read-string "Element name: "
                                  "para" 'docbook-element-name-history)))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))

        (while (not done)
          (setq curpos (point))
          (forward-paragraph)
          ;;(message (format "curpos(%d) point(%d)" curpos (point)))
          (if (>= curpos (point))
              (progn
                (setq done t)))
          (backward-paragraph)
          (if (eq (char-after) ?\n)
              (goto-char (1+ (point))))

          (if (not done)
              (progn
                (insert (concat "<" elname ">\n"))
                (forward-paragraph)
                (insert (if (eq (char-before) ?\n)
                            (concat "</" elname ">\n")
                          (concat "\n</" elname ">")))
                ;;(message (format "pt(%d) pt-max(%d)" (point) (point-max)))
                (if (>= (point) (1- (point-max)))
                    (setq done t))
                )))))))


;;;
;;; Dired and dired-x setting
;;;

(require 'dired-x)

(when (locate-library "dired+")
  (require 'dired+))

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


;;;
;;; Launch view-mode when visiting other's file.
;;;
(defun file-uid (filename)
  (caddr (file-attributes (expand-file-name filename))))

(defun smart-view-mode ()
  (let ((file buffer-file-name))
    (if (not (null file))                 ; if buffer has file name,
        (let ((fuid (file-uid file)))     ;
          (and (not (null fuid))          ; if file exists,
               (not (eq fuid (user-uid))) ; if the user/owner differs,
               (not (eq (user-uid) 0))    ; if not root,
               (view-mode 1))))))         ; enable `view-mode'.

(add-hook 'find-file-hook 'smart-view-mode)



;;;
;;; cscope binding
;;;
;;; You need to install cscope(1) and xcscope.el to use below bindings
;;; Read xcscope.el packaged in cscope source tarball. It can be obtained
;;; from http://cscope.sourceforge.net/
;;;
(when (locate-library "xcscope")
  (require 'xcscope))

;;;
;;; Version Control
;;;
(global-set-key [(control x) (control q)] 'vc-toggle-read-only)


;;(split-window-horizontally)


;;;
;;; gnus (news/e-mail) accounts settings
;;;
(require 'smtpmail)
(require 'starttls)

(defvar default-imap-port 993
  "Default port number for the IMAP4 protocol")

(defvar default-imap-address "imap.gmail.com"
  "Default IMAP server address")

(defvar default-imap-stream 'ssl
  "Default IMAP connection method.  See possible value from
  `nnimap-stream'.")

(defvar default-pop3-ssl-port 995
  "Default port number for the POP3 protocol over TSL/SSL")

(defvar default-smtp-ssl-port 587
  "Default port number for the encrypted SMTP protocol.
Best used for `smtpmail-smtp-service' as the default value.")

(defvar default-smtp-port 25
  "Default port number for the SMTP protocol.
Best used for `smtpmail-smtp-service' as the default value.")

(defvar default-smtp-server "smtp.gmail.com"
  "Default SMTP server address")

(defvar company-firewall-on-effect nil
  "t if behind the infamous company firewall")

(when (string-match "^selune" system-name)
  (setq company-firewall-on-effect t))
  
;; Since `gnus-nntp-server' will override `gnus-select-method', force
;; `gnus-nntp-server' to nil.
(setq gnus-nntp-server nil)

;;(setq gnus-select-method '(nntp "news.kornet.net"))
;;(setq gnus-select-method '(nntp "public.teranews.com"))

;; The select method for `M-x gnus'.
(setq gnus-select-method '(nntp "news.easynews.com"))

(when company-firewall-on-effect
  ;; My company firewall does not allow out-going traffic except port 80/443.
  ;;
  ;; On machine inside of company, use alternative NNTP configuration.
  ;;
  ;; For external IMAP server, use ssh local port forwarding:
  ;;
  ;; localhost:8993 -> imap.gmail.com:993
  ;;
  (setq gnus-select-method '(nntp "proxy.news.easynews.com"
                                  (nntp-port-number 80))
        default-imap-port 8993
        default-imap-stream "network"
        default-imap-address "localhost"))

;; `C-u M-x gnus' will use the secondary select method.
;;(setq gnus-secondary-select-methods '(nntp "news.kornet.net"))
(setq gnus-secondary-select-methods
      `((nnfolder "")
        ;; (nnimap "cinsky"
        ;;         (nnimap-stream ,default-imap-stream)
        ;;         (nnimap-address "imap.gmail.com")
        ;;         (nnimap-server-port ,default-imap-port))
        ;; (nnimap "admin"
        ;;         (nnimap-stream ,default-imap-stream)
        ;;         (nnimap-address "imap.gmail.com")
        ;;         (nnimap-server-port ,default-imap-port))
        ))

;; If you need to use multiple SMTP accounts, read the
;; following articles:
;;
;;   http://linil.wordpress.com/2008/01/18/gnus-gmail/

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      mail-from-style nil
      user-full-name "Seong-Kook Shin"
      user-mail-address "cinsky@gmail.com"
      message-signature-file "~/.signature"
      smtpmail-debug-info t
      smtpmail-debug-verb t)

;;
;; TODO: 1. set T if the external `gnutls-cli' exists
;;       2. set nil if the external `starttls' exists
;;       3. show warning message that SMTP will be not working.
(setq starttls-use-gnutls t)

(if starttls-use-gnutls
    (let ((tls (locate-file "gnutls-cli" exec-path)))
      (if (and tls (file-executable-p tls))
          (setq starttls-gnutls-program tls)
        (progn
          (lwarn '(dot-emacs) :warning
                 "GNUTLS command is not found, SMTP may not work correctly")
          (setq starttls-use-gnutls nil)))))

(if (not starttls-use-gnutls)
    (let ((tls (locate-file "starttls" exec-path)))
      (if (and tls (file-executable-p tls))
          (setq starttls-program tls)
        (lwarn '(dot-emacs) :warning
               "STARTTLS command is not found, SMTP may not work correctly"))))


;; Extra argument to "gnutls-cli"
(setq starttls-extra-arguments nil)

(setq smtpmail-smtp-server default-smtp-server)
(setq smtpmail-smtp-service default-smtp-ssl-port)

;; SMTP Username and password is located in seperated file for the security.
;; The format of ~/.authinfo looks like:
;;
;;   machine imap.gmail.com login USER@gmail.com password PASSWORD port 993
;;   machine smtp.gmail.com login USER@gmail.com password PASSWORD port 587
;;
;; Make sure that ~/.authinfo has access mode 600.

(let ((netrc "~/.authinfo"))
  (if (file-readable-p netrc)
      (setq smtpmail-auth-credentials netrc)
    (lwarn '(dot-emacs) :warning
           "NETRC auth. file not exist, SMTP may not work correctly")))

(setq smtpmail-starttls-credentials `((,smtpmail-smtp-server
                                       ,default-smtp-ssl-port
                                       nil nil)))


(defun complete-contact-address-internal ()
  (let ((name (completing-read "address: "
                               my-google-contacts
                               nil 'confirm)))
    (if (string-match "@" name)
        name
      (let ((found (assoc name my-google-contacts))
            (nam (if (string-match "\\(.*?\\) *([^)]*) *$" name)
                     (match-string 1 name)
                   name)))
        (format "%s <%s>" nam (cdr found))))))

(defun complete-contact-address (&optional arg)
  (interactive "P")
  (let ((address (complete-contact-address-internal))
        (pos (point)))
    (save-restriction
      (save-match-data
        (goto-char (point-min))
        (re-search-forward (regexp-quote mail-header-separator)
                           (point-max) t)
        (beginning-of-line)
        (let ((header-sep (point)))
          (if (>= pos header-sep)
              (progn
                (goto-char (point-min))
                (re-search-forward "^To:" header-sep t))
            (goto-char pos))
          (beginning-of-line)
          (if (or (re-search-forward "^[^[:blank:]][^:]*:[[:blank:]]*$"
                                     (line-end-position) t)
                  (re-search-forward "^[[:blank:]]+$" (line-end-position) t))
              (insert address)
            (beginning-of-line)
            (re-search-forward "[,[:blank:]]*$" (line-end-position) t)
            (replace-match (format ", %s" address))))))))

(eval-after-load "sendmail"
  '(progn
     (define-key mail-mode-map [(meta return)] 'complete-contact-address)

     (let ((contacts (concat (file-name-as-directory user-emacs-directory)
                             "contacts.el")))
       (when (file-exists-p contacts)
         (load-file contacts)))))
    

(defmacro time-loop (n &rest body)
  "Return time before and after N iteration of BODY.

DO NOT USE THIS MACRO.  INSTEAD, USE `benchmark'."
  (declare (indent 1) (debug t))
  `(let ((t1 (current-time)))
     (dotimes (i ,n)
       ,@body)
     (time-subtract (current-time) t1)))

(defmacro save-font-excursion (face &rest body)
  "Save the :font property of given FACE during the execution of BODY."
  (declare (indent 1) (debug t))
  `(let ((oldfont (face-attribute ,face :font)) ret)
     (setq ret (progn ,@body))
     (or (string= oldfont (face-attribute ,face :font))
         (set-face-attribute ,face nil :font oldfont))
     ret))


;;;
;;; color-theme settings
;;;
(setq color-theme-history-max-length 32)

;; If non-nil, new color theme will undo all settings made by previous
;; theme.  Normally, this is a bad idea, since some color themes do
;; not provide all face attributes.  However, if you want to find your
;; favorite theme using `color-theme-apply-random' or
;; `color-theme-apply', setting this variable to t might help.
;;
;; (setq color-theme-is-cumulative nil)

(defvar color-theme-favorites '(color-theme-deep-blue
                                color-theme-cinsk-wood
                                color-theme-charcoal-black
                                color-theme-clarity
                                color-theme-comidia
                                color-theme-dark-blue2
                                color-theme-dark-laptop
                                color-theme-taylor
                                color-theme-billw
                                color-theme-robin-hood)
  "My favorite color theme list")

(defun color-theme-select-favorite (&optional arg)
  "Apply one color theme from `color-theme-favorites'.

If called with prefix arguments, it will undo all settings made
by previous color theme.  Otherwise the new theme is installed on
top of each other."
  (interactive "P")

  (let ((color-theme-is-cumulative (if arg nil color-theme-is-cumulative)))
    (let ((theme (color-theme-apply-random 'favorite-only)))
      (message "%s installed" theme))))

(defun color-theme-apply-random (&optional favorite-only frame)
  "Select random color theme.

If optional FAVORITE-ONLY is non-nil, select color theme from
only in the `color-theme-favorites'.  The color theme is applied
to FRAME (nil for current frame).

This function returns the name of the color theme in string."
  (let* ((theme-list (if favorite-only 
                         color-theme-favorites
                       color-themes))
         (selected (nth (random (length theme-list)) theme-list))
         (theme-func (if (consp selected) (car selected) selected))
         (theme-name (if (consp selected) 
                         (cadr selected) 
                       (symbol-name theme-func))))
    (with-selected-frame (or frame (selected-frame))
      (funcall theme-func)
      theme-name)))


(defun color-themes-next-symbol (theme)
  "Return the next color-theme symbol of THEME"
  (let ((found 0) (next nil))
    (catch 'found
      (mapcar (lambda (entry)
                (if (and (= found 1) (null next))
                    (progn (setq next (car entry))
                           (throw 'found t)))
                (if (eq (car entry) theme)
                    (setq found 1)))
              color-themes))
    (if (and (= found 1) (null next))
        (setq next (car (caddr color-themes)))
      next)))


(defun color-theme-apply (&optional arg)
  "Apply the color theme.

If the argument is :random, this applies any color theme randomly, 
or if the argument is :next, this applies the next color theme in the
installed color theme list.  or if the argument is a symbol indicates
the color-theme function, it applies that color theme."
  (cond ((fboundp arg)  (apply arg nil))
        ((eq arg :random)  (color-theme-apply-random))
        ((eq arg :next)	(let ((theme (color-theme-next-symbol)))
                          (apply theme nil)
                          (message "%s installed" (symbol-name theme))))
        (t (error "Wrong type of argument"))))

(defun color-theme-next-symbol ()
  "Return the next color-theme symbol of the last applied color theme.

This function works iff color-theme-history-max-length is not NIL"
  (if (null color-theme-history)
      (car (car color-themes))
    (color-themes-next-symbol (car (car color-theme-history)))))


(when (and window-system
           (locate-library "color-theme"))
  (require 'color-theme)
  (and (fboundp 'color-theme-initialize)
       (color-theme-initialize))

  (and (locate-library "pink-bliss")
       (require 'pink-bliss))

  (and (locate-library "cinsk-wood")
       (require 'cinsk-wood))

  (global-set-key [(control f1)] 'color-theme-select-favorite)
  (global-set-key [(control f2)] '(lambda ()
                                    (interactive)
                                    (color-theme-apply :next)))

  ;; If you want to select random color theme on every new frame,
  ;; uncomment this.
  ;; (add-hook 'after-make-frame-functions 'set-frame-color-theme)
  (add-hook 'after-make-frame-functions 
            (lambda (frame) (color-theme-apply-random 'favorite frame)))
  ;; color-theme-* is frame-local from now.
  (setq color-theme-is-global nil)

  (random t)
  ;; Select random color theme from my favorite list
  (color-theme-select-favorite)
)


;;;
;;; YAML mode
;;;
(when (locate-library "yaml-mode")
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))


;;;
;;; CSS mode
;;;
(eval-after-load "css-mode"
  '(setq cssm-indent-function #'cssm-c-style-indenter))
(autoload 'css-mode "css-mode" "CSS editing major mode" t)
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))


;;;
;;; htmlize
;;;
(setq htmlize-convert-nonascii-to-entities nil)


;;;
;;; Calender
;;;
(require 'calendar)

(global-set-key [(control f12)] 'calendar)

(let ((my-diary-file (concat (file-name-as-directory user-emacs-directory)
                             "diary")))
  (if (file-readable-p my-diary-file)
      (setq diary-file my-diary-file)))

(setq mark-holidays-in-calendar t)
(setq mark-diary-entries-in-calendar t)
(add-hook 'diary-display-hook 'fancy-diary-display)

(setq local-holidays
      '((holiday-fixed 11 1 "삼성전자 창립일")))

(let ((cal-korea-path (expand-file-name 
                       (concat (file-name-as-directory user-emacs-directory)
                               "cal-korea-x"))))
  (when (file-accessible-directory-p cal-korea-path)
    (add-to-list 'load-path cal-korea-path)
    (when (locate-library "cal-korea-x")
      (require 'cal-korea-x)
      (setq holiday-general-holidays cal-korea-x-korean-holidays))))


;;;
;;; Org mode
;;;
(eval-after-load "org"
  '(progn
     (define-key outline-mode-map [(control down)]
       'outline-next-visible-heading)
     (define-key outline-mode-map [(control up)] 
       'outline-previous-visible-heading)
     (define-key outline-mode-map [(control shift down)]
       'outline-forward-same-level)
     (define-key outline-mode-map [(control shift up)]
       'outline-backward-same-level)

     ;; Rebind `org-force-cycle-archived' from "C-<TAB>" to "C-x C-<TAB>"
     ;; since I use "C-<TAB>" for `smart-other-window'.
     (move-key org-mode-map [(control tab)] [(control x) (control tab)])

     ;; Move the binding of `org-deadline' from "C-c C-d" to "C-c
     ;; C-S-d", since I'vd used the keybinding for
     ;; `delete-chars-forward-with-syntax'.
     (move-key org-mode-map [(control ?c) (control ?d)]
               [(control ?c) (control shift ?d)])

     (define-key org-mode-map [(control c) (control ?\\)]
       'org-table-convert-from-lines)

     (define-key org-mode-map [(control c) ?t] 'org-todo)
     ;; When opening a link with `org-open-at-point' (C-c C-o), These
     ;; settings allow to use acroread for pdf files and to use ggv
     ;; for ps files.
     (add-to-list 'org-file-apps '("pdf" . "acroread %s"))
     (add-to-list 'org-file-apps '("ps" . "ggv %s"))))

(require 'org-install)

;; Org mode requires font-locking on every org buffer
;; Since I use global-font-lock-mode, below sexp is not necessary.
;;
;; (add-hook 'org-mode-hook 'turn-on-font-lock)

;; org-hide-leading-stars should be set before loading org-mode.
(setq org-hide-leading-stars t)
(setq org-odd-levels-only t)
(setq org-agenda-include-diary t)

;; If org file loaded with folding, comparing files with ediff
;; is very unhandy, thus starting with everything is shown
(setq org-hide-block-startup nil)
(setq org-startup-folded 'showeverything)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(global-set-key [(control c) ?a] 'org-agenda)
(global-set-key [(control c) ?l] 'org-store-link)
(global-set-key [(control c) ?b] 'org-iswitchb)
(global-set-key [(control c) ?\"] 'org-capture)

(org-remember-insinuate)
(global-set-key [f8] 'org-capture)

(let* ((org-path (getenv "ORG_PATH"))
       (my-org-directory (if org-path 
                             org-path 
                           (concat (file-name-as-directory 
                                    user-emacs-directory)
                                   "agenda"))))
  ;; All of my org agena files are located in `my-org-directory'.
  (if (not (file-accessible-directory-p my-org-directory))
      (if (yes-or-no-p
           (format "create org directory(%s)? " my-org-directory))
          (make-directory my-org-directory t)))

  (if (file-accessible-directory-p my-org-directory)
      (let ((notefile (concat (file-name-as-directory my-org-directory)
                              "notes.org")))
        ;; Install all .org files in `my-org-directory' if exists
        (setq org-agenda-files
              (directory-files my-org-directory t ".*\\.org\\'"))
        (setq org-default-notes-file notefile)
        (setq org-directory my-org-directory))
    (lwarn '(dot-emacs) :warning
           (format "cannot access org files in %s." my-org-directory))))

(setq org-capture-templates
      '(("w" "Work-related TODO" entry
         (file+headline (concat (file-name-as-directory org-directory)
                                "work.org")
                        "Tasks")
         "* TODO %? %T\n  %i\n  %a")
        ("p" "Personal Project-related TODO" entry
         (file+headline (concat (file-name-as-directory org-directory)
                                "pproject.org")
                        "Tasks")
         "* TODO %? %T\n  %i\n  %a")
        ("P" "Personal TODO" entry
         (file+headline (concat (file-name-as-directory org-directory)
                                "personal.org")
                        "Tasks")
         "* TODO %? %T\n  %i\n  %a")
        ("j" "Personal Journal (date based)" entry
         (file+datetree (concat (file-name-as-directory org-directory)
                                "personal.org"))
         "* %?\n  Entered on %U\n  %i\n  %a")))
      
;; (add-to-list 'org-agenda-files "~/.emacs.d/personal.org")

(defvar org-table-convert-last-nrows	3
  "Default number of columns per row.  This is changed if user used
another value")

(defun org-table-convert-from-lines (&optional nrows)
  "Convert lines to the org table. Each line contains one column
so that users need to specify the number of columns per row.

For example, if the region contains 9 lines and each line
contains the digit from 1 to 9, calling
`org-table-convert-from-lines' with the column number 3 makes the
following:

| 1 | 2 | 3 |
| 4 | 5 | 6 |
| 7 | 8 | 9 |"
  (interactive "P")
  (require 'org)
  (if (null nrows)
      (let ((nrows (string-to-number
                    (read-string
                     (format "Number of columns per row[%d]: " 
                             org-table-convert-last-nrows)
                     nil nil 
                     (number-to-string org-table-convert-last-nrows)))))
        (setq org-table-convert-last-nrows nrows)
        (save-excursion
          (save-restriction
            (let ((start (set-marker (make-marker) (region-beginning)))
                  (end (set-marker (make-marker) (region-end))))
              ;;(message "nrows(%S) start(%S) end(%S)" nrows start end)
              (set-marker-insertion-type end t)
              (narrow-to-region start end)
              (goto-char start)
              (while (progn
                       (dotimes (i (1- nrows))
                         (end-of-line) (zap-to-nonspace) (insert "\t"))
                       (beginning-of-line)
                       (and (eq (forward-line) 0) (< (point) end))))
              (org-table-convert-region start end '(16))))))))


;;;
;;; Emacs-wiki support
;;;
;; (require 'emacs-wiki)


;;;
;;; ispell(aspell) configuration
;;;
;;; Currently neither of them provides Korean dictionary.
;;; Currently, ispell complained that it does not have proper dictionary in
;;; Korean language environment. 
(eval-after-load "ispell"
  '(progn
     (setq ispell-dictionary "english")))



;;;
;;; Do not display splash screen on startup
;;;

;; Show the `*scratch*' buffer (even in the presence of command-line
;; arguments for files)
;;
;; (setq initial-buffer-choice t)

;; Disable the startup screen
(setq inhibit-splash-screen t)



;;;
;;; ERC (IRC client) settings
;;;

(when (locate-library "erc")
  (eval-after-load "erc"
    '(progn
       (setq erc-default-coding-system '(cp949 . undecided))
       (setq erc-nick '("cinsk" "cinsky" "cinsk_" "cinsk__"))
       (setq erc-user-full-name "Seong-Kook Shin")
       (setq erc-server "localhost:8668"))))


;;;
;;; erlang configuration
;;;



(when (locate-library "erlang-start")
  (setq erlang-cinsk-init nil)  ; for one time init
  ;; Note on key-binding of erlang-mode.
  ;;
  ;; `erlang-mode-map' is initialized inside of `erlang-mode'.  This
  ;; means that we cannot use `erlang-mode-map' in any of
  ;; `erlang-load-hook', nor `eval-after-load'.
  ;;
  ;; It may be possible that call `erlang-mode-commands' directly,
  ;; but I don't want to do that, in case of future modification.
  (add-hook 'erlang-mode-hook
            (lambda ()
              (unless erlang-cinsk-init
                (define-key erlang-mode-map [(control ?c) ?b]
                  'erlang-compile)
                (define-key erlang-mode-map [(control ?c) ?\!]
                  'erlang-shell-display)
                (setq erlang-cinsk-init t))))
  (require 'erlang-start))



;;;
;;; Ruby Mode
;;;
(when (locate-library "ruby-mode")
  ;; For Emacs 24, install ruby-mode and inf-ruby-mode via package.el
  (require 'ruby-mode)

  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist
               '("[rR]akefile" . ruby-mode))

  (add-to-list 'interpreter-mode-alist  '("ruby" . ruby-mode))

  ;; Above configuration works on even ruby-mode 1.0, which is
  ;; distributed with Emacs 24 build-in.

  (when (locate-library "inf-ruby")
    (require 'inf-ruby))

  (when (fboundp 'inf-ruby-keys)
    ;; Unlike most major modes, key-bindings of ruby-mode (at
    ;; least 1.8.6) is done by `inf-ruby-keys', which is called
    ;; from `ruby-mode-hook' in the current implementation of
    ;; 'ruby-mode'.
    ;;
    ;; Thus, using `define-key' on `ruby-mode-map' is not working,
    ;; unless we call `define-key' in the `ruby-mode-hook' after
    ;; `inf-ruby-keys'.
    (add-hook 'ruby-mode-hook 'inf-ruby-keys)
    (add-hook 'ruby-mode-hook
              (lambda ()
                (define-key ruby-mode-map [(control ?c) ?\!] 'run-ruby)
                (define-key ruby-mode-map [(control ?c) (control ?c)] 
                  'comment-region)
                (define-key ruby-mode-map [(control ?c) (control ?b)]
                  'ruby-send-buffer)
                (define-key ruby-mode-map [(control ?c) (control ?e)] 
                  'ruby-send-block))
              ;; Make sure that this function is appended, not prepended
              'append))

  (if (fboundp 'ruby-send-buffer)
      (lwarn '(dot-emacs) :warning
             "`ruby-send-buffer' already defined.  need to update init.el")
    (defun ruby-send-buffer ()
      "Send the current buffer to the inferior Ruby process."
      (interactive)
      (save-excursion
        (save-restriction
          (widen)
          (ruby-send-region (point-min) (point-max))))))

  ;;
  ;; RVM irb uses its own prompt pattern which does not work properly 
  ;; with ruby-mode.  There are two ways to resolve this.  One is to modify
  ;; ruby-mode so that it can recognize RVM prompt pattern.  Another one is
  ;; to modify $HOME/.irbrc to force RVM irb to use the original prompt pattern.
  ;;
  ;; [http://bugs.ruby-lang.org/issues/6950]
  ;;
  ;; Since it is not the defect of ruby-mode, I decided to use $HOME/.irbrc
  ;; way.  You may need to insert following sentence in your .irbrc:
  ;;
  ;;   IRB.conf[:PROMPT_MODE] = :DEFAULT
  ;;
  ;; I'll leave the first solution as comments here in case of needs:
  ;;
  ;; The first prompt of Ruby 1.8 irb looks like "irb(main):001:0> ".
  ;; Depending on the current string token, the subsequent prompts
  ;; look like one of:
  ;; 
  ;;   irb(main):001:0> _
  ;;   irb(main):005:0* _
  ;;   irb(main):006:0' _
  ;;   irb(main):007:0" _
  ;;
  ;; The first prompt of Ruby 1.9 irb looks like "ruby-1.9.2-p180 :001 > ".
  ;; And subsequent prompts look like one of:
  ;;
  ;;   ruby-1.9.2-p180 :001 > _
  ;;   ruby-1.9.2-p180 :002 >    _
  ;;   ruby-1.9.2-p180 :003"> _
  ;;   ruby-1.9.2-p180 :004'> _
  ;;
  ;; ----
  ;;
  ;; (and (string-equal inferior-ruby-first-prompt-pattern
  ;;                    "^irb(.*)[0-9:]+0> *")
  ;;      (setq inferior-ruby-first-prompt-pattern
  ;;            "^\\(?:irb(.*)[0-9:]+0\\|ruby[-0-9.a-z]+ *:[0-9]+ *\\)> *"))
  ;; (and (string-equal inferior-ruby-prompt-pattern
  ;;                    "^\\(irb(.*)[0-9:]+[>*\"'] *\\)+")
  ;;      (setq inferior-ruby-prompt-pattern
  ;;            (concat "^\\(?:\\(irb(.*)[0-9:]+[>*\"'] *\\)+\\|"
  ;;                    "ruby[-0-9.a-z]+ *:[0-9]+[ \"']> *\\)")))
  )



;;;
;;; python-mode configuration
;;;
;;; Note that this configuration is for `python-mode.el' not for
;;; `python.el' in GNU Emacs distribution.

;;;
;;; Note that `python-mode' configuration must come after `ruby-mode'
;;; configuration.  Otherwise, `python-mode' will set
;;; `ruby-indent-level' to nil, thus indentation of ruby code will
;;; failed.
;;;

;;
;; In python-mode 5.1.0, autoloading `python-mode' causes `eval-after-load'
;; failed.  Don't know why
;;
;;(autoload 'python-mode "python-mode" "Python editing mode." t))

(eval-after-load "python-mode"
  '(progn
     ;; C-c C-b py-execute-buffer
     ;; C-c C-r py-execute-region
     ;; C-c C-e py-execute-string
     ;;
     ;; C-c C-c py-comment-region
     ;; C-c C-i py-indent-region
     ;;
     ;; C-c [   py-shift-region-left
     ;; C-c ]   py-shift-region-right

     (let ((map (if (boundp 'python-mode-map)
                    python-mode-map
                  py-mode-map)))
       (define-key map [(control ?c) ?\]] 
         'py-shift-region-right)
       (define-key map [(control ?c) ?\[] 
         'py-shift-region-left)

       ;; To eval string/region/buffer in native python,
       ;; use py-execute-(string|region|buffer).
       ;;
       ;; To eval in ipython, use py-execute-(string|region|buffer)-ipython.

       (if (and (executable-find "ipython")
                (fboundp 'py-execute-region-ipython))
           (progn
             (define-key map [(control ?c) (control ?b)]
               'py-execute-buffer-ipython)
             (define-key map [(control ?c) (control ?r)]
               'py-execute-region-ipython)
             ;; py-execute-string-ipython is not provided yet
             ;; (python-mode 6.0.10)
             (define-key map [(control ?c) (control ?e)]
               'py-execute-string))
         (progn
           (define-key map [(control ?c) (control ?b)]
             'py-execute-buffer)
           (define-key map [(control ?c) (control ?r)]
             'py-execute-region)
           (define-key map [(control ?c) (control ?e)]
             'py-execute-string)))

       (define-key map [(control ?c) (control ?c)] 'py-comment-region)
       (define-key map [(control ?c) ?i] 'py-indent-region)

       (when (locate-file "pychecker" exec-path)
         (define-key map [(control ?c) ?c] 'py-pychecker-run))

       ;; python-mode uses `C-c C-d' for `py-pdbtrack-toggle-stack-tracking'
       (define-key map [(control ?c) (control ?d)] 'zap-to-nonspace))))

(when (locate-library "python-mode")
  (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
  (setq interpreter-mode-alist (cons '("python" . python-mode)
                                     interpreter-mode-alist))
  (require 'python-mode))

;; ipython.el does not work with python-mode any longer. And since
;; python-mode provides an interface to ipython, I'll stick to
;; python-mode only from now on. -- cinsk
  

;;;
;;; Maven
;;;

(when (not (assoc 'maven3 compilation-error-regexp-alist-alist))
  ;; On GNU Emacs 24.0.94.1 (x86_64-apple-darwin, NS
  ;; apple-appkit-1038.36) of 2012-02-28 on bob.porkrind.org,
  ;; http://emacsformacosx.com/builds/Emacs-pretest-24.0.94-universal-10.6.8.dmg,
  ;; maven in `compilation-error-regexp-alist-alist' is not handling
  ;; maven 3.0.3 in my Macbook.  -- cinsk

  (add-to-list 'compilation-error-regexp-alist-alist
               '(maven3 "^\\[ERROR\\] +\\(.+?\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\].*"
                       1 2 2)
  (add-to-list 'compilation-error-regexp-alist 'maven3)
  ;// "[ERROR] /Users/.../KafkaBridge.java:[71,52] ';' expected"
))
  


;;;
;;; w3m
;;;
(when (locate-library "w3m")
  (require 'w3m-load))


(setq w3m-use-cookies t)


;;;
;;; browse-url configuration
;;;
(require 'browse-url)

(defun browse-url-w3m (url &optional new-window)
  (interactive (browse-url-interactive-org "W3M URL: "))
  (w3m url))

(cond ((memq system-type '(windows-nt ms-dos cygwin))
       ;; Use system default configuration
       nil)

      ((or (display-graphic-p) 
           (= (call-process-shell-command "xset q") 0))
       ;; Even if (display-graphic-p) returns nil,
       ;; it may be possible to launch X application.
       (cond ((executable-find "chromium")
              (setq browse-url-browser-function 'browse-url-generic
                    browse-url-generic-program (executable-find "chromium")))
             ((executable-find browse-url-firefox-program)
              (setq browse-url-browser-function 'browse-url-firefox))))

      ((fboundp 'w3m)
       (setq browse-url-browser-function 'browse-url-w3m)))

(add-to-list 'browse-url-filename-alist
             '("\\`/home/\\([^/]+\\)/public_html/\\(.*\\)\\'" . 
               "http://localhost/~\\1/\\2"))

;; gentoo: /var/www/localhost/htdocs
;; ubuntu: /var/www/
;; centos: /var/www/html/  /var/www/cgi-bin/
(add-to-list 'browse-url-filename-alist
             '("\\'/var/www/localhost/htdocs/\\(.*\\)\\'" . 
               "http://localhost/\\1"))


;;;
;;; gnuplot
;;;
(when (locate-library "gnuplot")
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)

  (setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode))
                                auto-mode-alist)))


;;;
;;; go lang
;;;
(let* ((godir "/usr/local/go")
       (gldir (concat (file-name-as-directory godir) "misc/emacs")))
  (when (file-accessible-directory-p gldir)
    (add-to-list 'load-path gldir 'append)
    (when (locate-library "go-mode-load")
      (require 'go-mode-load))))



;;;
;;; lua
;;;
(when (locate-library "lua-mode")
   (autoload 'lua-mode "lua-mode" "Major mode for lua script")
   (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))

;;;
;;; YASnippet -- http://code.google.com/p/yasnippet/
;;;
(when (locate-library "yasnippet")
  (require 'yasnippet)
  ;;(yas/initialize)

  ;; The official document describes the snippets directory in
  ;; "~/.emacs.d/plugins/yasnippet-x.y.z/snippets", whereas Gentoo
  ;; yasnippet package installed them in
  ;; "/usr/share/emacs/etc/yasnippet/snippets".
  (let ((dir (catch 'found
               (mapc (lambda (path)
                       (when (file-accessible-directory-p path)
                         (setq snippet-dir path)
                         (throw 'found path)))
                     (list "/usr/share/emacs/etc/yasnippet/snippets"
                           "~/.emacs.d/plugins"
                           (file-name-directory
                            (locate-library "yasnippet")))))))
    (if dir
        (progn (setq yas/root-directory dir)
               (yas/load-directory yas/root-directory))
      (lwarn '(dot-emacs) :warning
             "yasnippet cannot find the snippet directory"))))


;;;
;;; Scala
;;;

;; Currently, Scala 2.8.x is not provided by gentoo portage. Thus, I
;; will use the binary distribution from the Scala repository in
;; /opt/scala
(when (not (locate-library "scala-mode-auto"))
  (let* ((scala-mode-path "/opt/scala/misc/scala-tool-support/emacs")
         (scala-file (concat (file-name-as-directory scala-mode-path)
                             "scala-mode-auto.el")))
    (if (file-exists-p scala-file)
        (add-to-list 'load-path scala-mode-path))))

(when (locate-library "scala-mode-auto")
  (eval-after-load "scala-mode"
    '(progn
       ;; Modify scala-mode-map to keep consistency with other
       ;; interpreter setting.
       ;;
       ;; C-c C-b   scala-eval-buffer
       ;; C-c C-r   scala-eval-region
       ;; C-c C-e   scala-eval-definition  (TODO: check the symantics)
       (define-key scala-mode-map [(control ?c) (control ?e)]
         'scala-eval-definition)
       ;; 
       ;; scala-undent-line: `C-<tab>' -> `<backtab>'
       (move-key scala-mode-map
                 [(control tab)] [backtab])

       (define-key scala-mode-map [(control ?c) ?\!] 'scala-run-scala)
       ))
  (require 'scala-mode-auto))


;;;
;;; ESS(Emacs Speaks Statistics) setting for R.
;;;
(when (locate-library "ess-site")
  (require 'ess-site))


;;; To save & load Emacs session, following lines should be the last
;;; line in this file.
;;;
;;; The first time you save the state of the Emacs session, you must
;;; do it manually, with the command `M-x desktop-save'. Once you have
;;; done that, exiting Emacs will save the state again--not only the
;;; present Emacs session, but also subsequent sessions. You can also
;;; save the state at any time, without exiting Emacs, by typing `M-x
;;; desktop-save' again.
;;;
;;; In order for Emacs to recover the state from a previous session,
;;; you must start it with the same current directory as you used when
;;; you started the previous session.  This is because `desktop-read'
;;; looks in the current directory for the file to read.  This means
;;; that you can have separate saved sessions in different
;;; directories; the directory in which you start Emacs will control
;;; which saved session to use.

;;(desktop-load-default)
;;(desktop-read)


;;; I frequently uses `narrow-to-region', which is disabled by default
;;; because it confuse users who do not understand it.  If you do not
;;; use it or do not understand it, comment below lines.
(put 'narrow-to-region 'disabled nil)


;;;
;;; GNU Emacs Calculator Configuration
;;;
(autoload 'calc "calc" "The Emacs Calculator" t)
(global-set-key [f10] 'calc)
(global-set-key [(control f10)] 'quick-calc)



;;(global-set-key [f2] 'ff-find-other-file)
;;(global-set-key [f3] 'dired-jump)

(require 'dired-x)

;;;
;;; elscreen
;;;
(eval-after-load "elscreen"
  '(progn
     (define-key elscreen-map "\C-z" 'elscreen-toggle)
     (setq elscreen-display-screen-number nil)
     ))

(when nil
  ;; Don't know why, but in my system configuration,
  ;; when Emacs 23.1.50 autoloads elscreen 1.4.6, launching emacs
  ;; with filename causes "Symbol's value as variable is void: dir" error.
  (when (locate-library "elscreen")
    (require 'elscreen)))


;;;
;;; ecb settings; I do not use ECB any more -- cinsk.
;;;
(when nil
  (when window-system
    (autoload 'ecb-activate "ecb" "Emacs Code Browser" t)
    (eval-after-load "ecb"
      '(progn
         (setq ecb-toggle-layout-sequence
               '("left3" "left-symboldef" "left8"))
         (setq ecb-tip-of-the-day nil)
         (set-face-font 'ecb-default-general-face
                        "-*-helvetica-medium-r-*-*-12-*-*-*-*-*-*-*")
         )))

  (defun ecb-next-action (arg)
    (interactive "P")
    (or (featurep 'ecb)
        (progn (require 'cedet)
               (require 'ecb)))         ; load required packages
    (cond ((null ecb-minor-mode) (ecb-activate))
          (t (if (null arg) 
                 (ecb-toggle-layout)
               (ecb-deactivate)))))

  ;;(global-set-key [f11] 'ecb-next-action)
  )

;;; Local Variables:
;;; coding: utf-8
;;; End:
