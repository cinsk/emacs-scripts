;; -*-emacs-lisp-*-

;;;
;;; Mac OS customization
;;;

;; (require 'ucs-normalize)
;; (set-file-name-coding-system 'utf-8-hfs)

(defun darwin-smart-other-frame (&optional arg)
  "Switch to other frame or call `tmm-menubar`."
  (interactive "P")
  (if (display-graphic-p)
      (other-frame (or arg 1))
    (tmm-menubar arg)))

(defun set-exec-path-from-shell-path ()
  (let ((newpath (shell-command-to-string "/bin/sh -l -c 'echo $PATH'")))
    (setenv "PATH" newpath)
    (setq exec-path (split-string newpath path-separator))))

(defun macos-make-sub-frame ()
  (interactive)
  (let ((frame (make-frame)))
    ;; (scale-default-font-height 0.9 frame)
    ;; (set-frame-parameter frame 'height 54)
    (set-frame-parameter frame 'left '(- 0))
    (set-frame-parameter frame 'top '(- 5000))))

;; Disable autoface mode that uses variable-width font for text-mode
;; derived modes.
(when (fboundp 'aquamacs-autoface-mode)
  (aquamacs-autoface-mode -1))

;; Aquamacs had '*scratch*' buffer in text mode; force it having lisp mode
(setq initial-major-mode 'lisp-interaction-mode)

(cond ((boundp 'aquamacs-version)
       ;; These are Aquamacs specific configuration

       ;; `mac-command-modifier' is set to 'alt by default.
       ;; if we override it before `mac-options-modifier' points `alt,
       ;; some of the Aquamacs specific shortcut will be ruined.
       (setq mac-option-modifier 'alt)
       (setq mac-command-modifier 'meta)

       (setq select-enable-clipboard t)

       ;; Aquamacs has `cursor-type' to bar which is hardly visible.
       (setq-default cursor-type 'box)

       (setq special-display-regexps
             (remove "[ ]?\\*info.*\\*[ ]?"
                     (remove "[ ]?\\*[hH]elp.*" special-display-regexps)))

       ;; CMD-}   next-tab-or-buffer
       ;; CMD-{   previous-tab-or-buffer
       ;; CMD-OPT-1 aquamacs-join-windows
       ;; CMD-OPT-2 aquamacs-split-windows-vertically
       ;; CMD-C-F   aquamacs-toggle-full-frame

       ;; (setq same-window-regexps nil)
       )
      (t ; vanilla Emacs on MacOS (darwin)
       (setq mac-option-modifier 'alt)
       (setq mac-command-modifier 'meta)
       (setq select-enable-clipboard t))
)



;; sets fn-delete to be right-delete
(global-set-key [kp-delete] 'delete-char)

;;(set-exec-path-from-shell-path)

(when nil

  ;; As homebrew provides a way to override default binaries using PATH,
  ;; Below code no longer necessary.
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
    (setq insert-directory-program "/usr/local/bin/ls"))))


(defun darwin/display-list ()
  "List display resolution of darwin.

Each element has the form (WIDTH . HEIGHT) in pixel."
  ;; I'm not sure why I wrote this instead of `display-pixel-width' and
  ;; `display-pixel-height'.
  (let (result)
    (with-temp-buffer
      (shell-command "system_profiler SPDisplaysDataType | grep -i resolution"
                     'current-buffer nil)
      (goto-char (point-min))
      (while (re-search-forward
              "\\([[:digit:]]*\\) *x *\\([[:digit:]]*\\)" nil 'noerror)
        (let ((width (string-to-number (match-string-no-properties 1)))
              (height (string-to-number (match-string-no-properties 2))))
          (setq result (cons (cons width height)
                             result)))))
    (nreverse result)))

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

  (when (locate-library "fontutil")
    (require 'fontutil))

  (when (eq 0 (string-to-number (or (getenv "EMACS_NOFONT") "0")))
    (let ((displays (darwin/display-list)))
      (if (or (> (length displays) 1)
              (not (null (cl-find-if (lambda (x) (>= (car x) 3000))
                                     displays))))
          (fontutil/set-font "pt-16")
        (fontutil/set-font "pt-14")))))


(setq default-frame-alist (append default-frame-alist
                                  '((width . 80) (height . 45)
                                    ;;(line-spacing . 2)
                                    (alpha . (100 . 90)))))

;; (when (display-graphic-p)
;;   (global-set-key [(meta ?c)] 'ns-copy-including-secondary))

(global-set-key [(meta ?`)] 'darwin-smart-other-frame)

;; If Emacs is not launched in Terminal, .bashrc is not executed, so
;; that /usr/local/bin is not added to the PATH, so that Emacs will
;; not find some executables in "/usr/local/bin".
(unless (member "/usr/local/bin" exec-path)
  (add-to-list 'exec-path "/usr/local/bin"))

(with-eval-after-load "tex"
  (TeX-global-PDF-mode)
  (add-to-list 'TeX-command-list
               '("View" "open %o"
                 TeX-run-discard-or-function t t :help "Run Viewer")))

(setq cinsk/ediff-wide-display-policy 'fullscreen)

(when nil
  ;; By placing /usr/local/bin prior to /usr/bin, below snippets are
  ;; not necessary
  (when (file-executable-p "/usr/local/bin/diff3")
    (setq ediff-diff3-program "/usr/local/bin/diff3"
          ediff-diff-program  "/usr/local/bin/diff"
          ediff-cmp-program "/usr/local/bin/cmp"))

  (when (file-executable-p "/usr/local/bin/patch")
    (setq ediff-patch-program "/usr/local/bin/patch")))

;; (desktop-save-mode 1)


;; `Info-additional-directory-list' is not a variable but a
;; customizable variable.  `add-to-list' may fail if it is not defined
;; yet.
(if (boundp 'Info-additional-directory-list)
    (add-to-list 'Info-additional-directory-list "/usr/local/share/info")
  (setq Info-additional-directory-list '("/usr/local/share/info")))


(defun browse-url-mac-osx-open (url &rest args)
  (shell-command (format "/usr/bin/open %s" url)))

(setq browse-url-mailto-function #'browse-url-mac-osx-open)


(defun darwin-dictionary-look-up (&optional word)
  "Look up WORD using Mac Dictionary Application.

If WORD is nil, try to get a word from the current region if active,
then try to use a word at point."
  (interactive)
  (let ((use-empty-active-region nil))
    (let ((w (or word
                 (and (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end)))
                 (thing-at-point 'word))))
      (if (or (null w) (eq (length w) 0))
          (message "Nothing at point nor nothing in the region")
        (message "Looking up '%s'..." w)
        (call-process "/usr/bin/open" nil nil nil (format "dict://%s" w))))))

(global-set-key [(alt ?d)] 'darwin-dictionary-look-up)

(with-eval-after-load "dired-x"
  (setq dired-guess-shell-alist-user
        (append dired-guess-shell-alist-user
                '(("\\.xbm\\'" "open") ; You may need to install a viewer application like ToyViewer.
                  ("\\.png\\'" "open")
                  ("\\.jpe?g\\'" "open")
                  ("\\.tiff?\\'" "open")
                  ("\\.pdf\\'" "open")
                  ("\\.gnuplot\\'" "gnuplot")
                  ("\\.svg\\'" "open")
                  ("\\.e?ps\\'" "open")
                  ("\\.docx?\\'" "open")
                  ("\\.pptx?\\'" "open")
                  ("\\.xlsx?\\'" "open")
                  ("\\.dmg\\'" "open")
                  ("\\.pkg\\'" "open")
                  ("\\.otf\\'" "open")
                  ("\\.ttf\\'" "open")
                  ("\\.msg\\'" "msgconvert") ; https://github.com/mvz/email-outlook-message-perl
                  ("\\.mobi\\'" "open -a ebook-viewer *") ; Install Calibre for ebook-viewer
                  ("\\.e?ps\\.g?z\\'" "gunzip -qc * | open -a Preview -f")
                  ("\\.e?ps\\.Z\\'" "zcat * | open -a Preview -f")
                  ))))

;; Darwin's default shell is zsh(1).  To prevent shell commands
;; echoing, this is required.
(when (string-equal (file-name-nondirectory shell-file-name) "zsh")
  (add-hook 'comint-mode-hook #'(lambda () (setq comint-process-echoes t))))

;; Darwin (MacOS Ventura) uses Control-Meta-D for dictionary lookup,
;; which is not able to override at the moment.  it is used for
;; `isearch-del-char', so we can use Option key instead of C-M-d.
(define-key isearch-mode-map (kbd "A-C-d") 'isearch-del-char)
