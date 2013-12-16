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
    ;; (scale-default-font-height 0.9 frame)
    ;; (set-frame-parameter frame 'height 54)
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

;; (setq fontconfig-data
;;       '("Inconsolata" . (:family "Inconsolata")))


(defvar cinsk/fontconfig
  '(("scodepro-14" . (((:family "Source_Code_Pro" :size 14)
                    (hangul :family "NanumGothicCoding" :size 16)
                    (symbol :family "Symbola" :size 15))))
    ("scodepro-15" . (((:family "Source_Code_Pro" :size 15)
                    (hangul :family "NanumGothicCoding" :size 18)
                    (symbol :family "Symbola" :size 15))))
    ("scodepro-16" . (((:family "Source_Code_Pro" :size 16)
                    (hangul :family "NanumGothicCoding" :size 20)
                    (symbol :family "Symbola" :size 15))))
    ("inconsolata-14" . (((:family "Inconsolata" :size 14)
                          (hangul :family "NanumGothicCoding" :size 14)
                          (symbol :family "Symbola" :size 15))))
    ("inconsolata-15" . (((:family "Inconsolata" :size 15)
                          (hangul :family "NanumGothicCoding" :size 16)
                          (symbol :family "Symbola" :size 15))))
    ("inconsolata-16" . (((:family "Inconsolata" :size 16)
                          (hangul :family "NanumGothicCoding" :size 16)
                          (symbol :family "Symbola" :size 15))))
    ("monaco-14" . (((:family "Monaco" :size 14)
                     (hangul :family "NanumGothicCoding" :size 16)
                     (symbol :family "Symbola" :size 15))))
    ("monaco-15" . (((:family "Monaco" :size 15)
                     (hangul :family "NanumGothicCoding" :size 18)
                     (symbol :family "Symbola" :size 15))))
    ("monaco-16" . (((:family "Monaco" :size 16)
                     (hangul :family "NanumGothicCoding" :size 20)
                     (symbol :family "Symbola" :size 15)))))
  "Font and Frame configuration list

Each element has a form of (FONT-SPEC FRAME-SPEC), where
FONT-SPEC is a list of font properties, and FRAME-SPEC is a list
of frame parameters.

See `font-spec' for the list of font properties.

You can specify multiple font in FONT-SPEC; In this case, FONT-SPEC
is a list of font specs, where the first element is font properties
for the default font, and the rest elements are for the additional
font properties.  Except the first element, all elements have
TARGET as a prefix element.  See `set-fontset-font' for the possible
TARGET.  For example, if you want to use font XXX as the default font,
and you want to use YYY for Korean script, hangul, and use ZZZ for
symbolic glyphs, then the FONT-SPEC might be

  ((:family \"XXX\" :size 14)
   (hangul :family \"YYY\" :size 16)
   (symbol :family \"ZZZ\") :size 15)

As you can see in above, each font can have different size.  (This
is useful for CJK fonts)")

(defun cinsk/apply-fontconfig (name)
  (let ((fonts (car (cdr (assoc name cinsk/fontconfig))))
        (params (cadr (cdr (assoc name cinsk/fontconfig)))))
    (unless (listp (car fonts))
      (setq fonts (list fonts)))
    (when (car fonts)
      (set-face-attribute 'default nil :font (apply 'font-spec (car fonts))))
    (dolist (aux (cdr fonts))
      (set-fontset-font t (car aux) (apply 'font-spec (cdr aux))))
    (when params
      (setq default-frame-alist params)
      (dolist (f (frame-list))
        (modify-frame-parameters f params)))))

(defun set-font (name)
  (interactive (list (completing-read "font: " cinsk/fontconfig)))
                                      ;; (lambda (f)
                                      ;;   (list-fonts (apply #'font-spec
                                      ;;                      (car (cdr f)))))
                                      ;; t nil)))
  (cinsk/apply-fontconfig name))

(when (display-graphic-p)
  ;; These configuration seems to work in
  ;; GNU Emacs 24.1.1 (x86_64-apple-darwin, NS apple-appkit-1038.36)
  ;; of 2012-06-11 on bob.porkrind.org

  ;; default font family
  ;; (set-face-attribute 'default nil :family "Monaco")
  ;; (set-face-attribute 'default nil :family "Inconsolata")

  ;; default font size
  ;;
  ;; WARNING: depending on the font family, some height value may
  ;; cause a broken frame display; that is, the beginning of the
  ;; buffer is not visible.
  ;; (set-face-attribute 'default nil :height 135)
  ;; (set-face-attribute 'default nil :height 160)

  ;;(set-fontset-font t 'unicode (font-spec :size 20.0))

  ;; You may add :size POINT in below font-spec if you want to use
  ;; specific size of Hangul font regardless of default font size
  (set-font "inconsolata-15"))

(setq default-frame-alist (append default-frame-alist
                                  '((width . 80) (height . 45)
                                    ;;(line-spacing . 2)
                                    (alpha . (100 . 85)))))

(when (display-graphic-p)
  (global-set-key [(meta ?c)] 'ns-copy-including-secondary))
(global-set-key [(meta ?`)] 'darwin-smart-other-frame)

;; If Emacs is not launched in Terminal, .bashrc is not executed, so
;; that /usr/local/bin is not added to the PATH, so that Emacs will
;; not find some executables in "/usr/local/bin".
(unless (member "/usr/local/bin" exec-path)
  (add-to-list 'exec-path "/usr/local/bin"))

;; (desktop-save-mode 1)
