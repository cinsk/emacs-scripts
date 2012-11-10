;; -*-emacs-lisp-*-

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

(defun macos-make-sub-frame ()
  (interactive)
  (let ((frame (make-frame)))
    (scale-default-font-height 0.9 frame)
    (set-frame-parameter frame 'height 54)
    (set-frame-parameter frame 'left '(- 0))
    (set-frame-parameter frame 'top '(- 5000))))
    

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

(when (display-graphic-p)
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

(desktop-save-mode 1)
