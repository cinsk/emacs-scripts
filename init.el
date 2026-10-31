;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;
;;; Seong-Kook Shin's .emacs initialization file.
;;;

;;;
;;; You can download the latest version of this script at cinsk.org
;;;
;;; $ # make sure that you don't have .emacs.d/ nor .emacs file
;;; $ rm -r .emacs.d .emacs
;;; $ # Get the source file from my repository
;;; $ git clone http://github.com/cinsk/emacs-scripts.git .emacs.d
;;;

;;; The default value of `gc-cons-threshold' is set to 800000 on
;;; 64-bit system.

;;; ==================================================================
;;; Base Initialization Configuration (directories, mandatory packages)
;;; ==================================================================

(defvar cinsk/gc-cons-threshold (* 10 gc-cons-threshold))
(setq gc-cons-threshold cinsk/gc-cons-threshold)
;;(setq garbage-collection-messages t)


(defun system--uname ()
  "Return system information."
  (let ((cmd (cond ((or (eq system-type 'gnu/linux)
                        (eq system-type 'darwin))
                    "uname -a")
                   ((eq system-type 'windows-nt)
                    "ver")
                   (t
                    nil))))
    (if cmd
        (string-trim (shell-command-to-string cmd))
      "")))

(defvar system-uname (system--uname)
  "system information")

(defun wsl-p ()
  (if (and (eq system-type 'gnu/linux)
           (string-match "[mM]icrosoft" system-uname))
      t))

;;; Load custom configuration -- Emacs's customizable variables will
;;; update this file if `custom-file' is nil which is not preferrable.
(setq custom-file "~/.emacs.d/my-custom.el")
(load custom-file t)

;;; emacs packages for my personal uses are placed in $HOME/.emacs.d

(use-package exec-path-from-shell
  ;; This package provides means to set some envirionment variables
  ;; from the shell, even if emacs was launched from Spotlight or
  ;; Dock.  In that case, the parent process of emacs is
  ;; launchd(8). (pid == 1)
  :when (eq system-type 'darwin)
  :ensure t
  :config
  (let ((bash "/opt/homebrew/bin/bash"))
    (when (and (file-executable-p bash)
               (eq (alist-get 'ppid (process-attributes (emacs-pid))) 1))
      ;; required for terminal / shell related packages
      (setenv "SHELL" bash)

      ;; Instruct `M-x shell' to use bash; Ideally, shell package will
      ;; set it from the value of SHELL environment variable.
      ;; However, I cannot guarantee when shell package initialization
      ;; is happening.  Thus, I'll set `shell-file-name' to bash here,
      ;; anyway.
      (setq shell-file-name bash)

      ;; Instruct exec-path-from-shell package to read from bash config
      (setq exec-path-from-shell-shell-name bash)))

  (exec-path-from-shell-initialize))

(when nil
  (when (string-equal (getenv "SHELL") "/bin/false")
    ;; "$SHELL" is set to "/bin/false" in darwin under Amazon User
    ;; Cert. System. -- Hmm. this seems not true.
    ;;  (let ((sh (or (executable-find "bash") "/bin/sh")))
    (let ((sh (or (executable-find "zsh") "/bin/zsh")))
      (setenv "SHELL" sh)
      (setq shell-file-name sh))))



(setq user-emacs-directory "~/.emacs.d/")

(when (and (not (file-accessible-directory-p user-emacs-directory))
           (not noninteractive))
  (if (yes-or-no-p
       (format "create user directory(%s)? " user-emacs-directory))
      (make-directory user-emacs-directory t)))

(setq initial-buffer-choice "~/.emacs.d/README.org")



;;; ---- package system initialization ----

;;;
;;; Whenever major upgrade for Emacs, try to execute below sexp to
;;; recompile everything in elpa/ directory.
;;;
;;;   (byte-recompile-directory package-user-dir nil 'force)
;;;

(with-eval-after-load "tls"
  ;; On recent Mac (Mac Mojave), the ca-bundle is not available from
  ;; the file system, so `gnutls-trustfiles' would return nil. If the
  ;; `tls-program' uses "--x509cafile nil" command-line, gnutls-cli
  ;; will fail, so I'm going to remove "--x509cafile" from the
  ;; command-line
  ;;
  ;; TODO: Is it secure enough?
  ;; To verify, run:
  ;;    (url-copy-file "https://www.google.com/" "google.txt")
  (add-to-list 'tls-program "gnutls-cli -p %p %h"))

(let ((pkgdir (file-name-concat user-emacs-directory "package")))
  ;; PKGDIR will contains the last emacs-23 compatible package.el from
  ;; https://github.com/technomancy/package.el
  (when (and (file-readable-p pkgdir)
             (= emacs-major-version 23))
    (add-to-list 'load-path pkgdir))

  (when (and (>= emacs-major-version 23)
             (locate-library "package"))
    (require 'package)

    (dolist (entry '(;; marmalade cert expired.
                     ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                     ;; ("sc" . "http://joseito.republika.pl/sunrise-commander/")
                     ("melpa" . "https://melpa.org/packages/")
                     ("melpa-stable" . "https://stable.melpa.org/packages/")
                     ;; ("gnu" . "https://elpa.gnu.org/packages/")
                     ))
      (add-to-list 'package-archives entry))
    (setq package-archive-priorities
          '(("gnu" . 5)
            ("melpa" . 0)
            ("melpa-stable" . 10)))
    ;; According to the package.el, if `package-enable-at-startup' is
    ;; t, it will load all the packages on start up.  But it didn't.
    ;; Perhaps it's a problem related to the version (currently Emacs
    ;; 23).  Thus, I'll force to load it here.
    (package-initialize)
    ;;
    (setq package-enable-at-startup t)))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-compute-statistics t)

(defun my/missing-use-packages ()
  "List `use-package' declarations whose library can't be found."
  (interactive)
  (let (missing)
    (dolist (f (append (list user-init-file
                             (expand-file-name "postinit.el" user-emacs-directory))
                       (directory-files (expand-file-name "init" user-emacs-directory)
                                        t "\\.el\\'")))
      (when (file-readable-p f)
        (with-temp-buffer
          (insert-file-contents f)
          (goto-char (point-min))
          (while (re-search-forward
                  "^[ \t]*(use-package[ \t\n]+\\([^ \t\n()]+\\)" nil t)
            (let ((pkg (match-string 1)))
              (unless (locate-library pkg)
                (push (format "%s (%s)" pkg (file-name-nondirectory f))
                      missing)))))))
    (message "Missing: %s"
             (if missing (string-join (nreverse missing) ", ") "none"))))

;;(unless (locate-library "s")
;;  (package-install 's))

;; I will install packages that is not managed by packages.el in
;; "$HOME/.emacs.d/local".
;;
;; Note that packages in this directory has higher priority than others.
(defvar user-custom-packages-directory
  (file-name-concat user-emacs-directory "local")
  "Manually installed packages are in this directory")


(let ((srcpath (expand-file-name (file-name-concat user-emacs-directory "src"))))
  (add-to-list 'load-path srcpath)
  )

(setq uinit-init-directory "~/.emacs.d/init"
      uinit-byte-compile-on-load nil)

;; (byte-recompile-directory package-user-dir nil 'force)

(require 'uinit)

(use-package enable-mmode)

;;; ==================================================================
;;; Packages Configuration
;;; ==================================================================

(use-package cinsk-common
  :config
  (keymap-global-set "C-c n" #'cinsk/line-numbers-on-region)
  (keymap-global-set "C-c q" #'cinsk/fill-text-line-paragraph)
  (keymap-global-set "M-Q" #'cinsk/unfill-paragraph)
  ;;(cinsk/add-site-lisp-directories "/usr/local/share/emacs/site-lisp")
  ;;(cinsk/add-site-lisp-directories user-custom-packages-directory)
  )

(use-package amz-common
  :disabled
  :config
  (when (boundp 'c-default-style)
    ;; Amazon library uses BSD style for C/C++, which I don't like
    (setq c-default-style (delq (assoc 'c++-mode c-default-style)
                                c-default-style)
          c-default-style (delq (assoc 'c-mode c-default-style)
                                c-default-style))))

(use-package amazon-util
  :disabled
  :requires amz-common)

(when (file-directory-p "/apollo/env/EmacsAmazonLibs/share/emacs/site-lisp")
  (add-to-list 'load-path "/apollo/env/EmacsAmazonLibs/share/emacs/site-lisp"))


(use-package capitalize+
  :config
  (substitute-key-definition 'capitalize-word 'capitalize-word+
                             (current-global-map)))

(use-package fontutil
  :config
  ;; FYI, changing UI components (fonts, colors), place the config in postinit.el
  (fontutil/install-mouse-wheel))

(use-package xskel)

(use-package hippie-exp
  ;; Use hippie expansion for dynamic abbreviation
  :config
  (keymap-global-set "M-/" #'hippie-expand)
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-line
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-list
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))


(use-package untabify
  :config
  (setq-default indent-tabs-mode nil)     ; do not insert tab character.
  (add-hook 'before-save-hook 'untabify-remove-trailing-spaces-on-write))


;;;
;;; Markdown mode
;;;
(use-package markdown-mode
  :config
  (add-to-list 'markdown-css-paths
               "http://thomasf.github.io/solarized-css/solarized-light.min.css")
  ;; On Emacs 27.1, if a displayed image is too tall, it prevent Emacs
  ;; scroll up/down.
  (when (or (memq 'scale (image-transforms-p))
            (image-type-available-p 'imagemagick))
    (let ((h (* 20 (line-pixel-height)))
          (w (floor (* (frame-inner-width) 0.7))))
      (and (<= h 0) (setq h 400))
      (and (<= w 0) (setq w 400))
      (setq markdown-max-image-size (cons w h)))))

(use-package cmake-mode
  :disabled
  :config
  (setq auto-mode-alist
        (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                  ("\\.cmake\\'" . cmake-mode))
                auto-mode-alist)))

(use-package winner
  ;; A history manager for window configuration.
  :config
  ;; `C-c left' for undo, `C-c right' for redo
  (winner-mode 1))

(use-package windmove
  :disabled
  :config
  ;; Select a window by pressing arrow keys.
  ;; The default modifier is 'shift which is not suitable for me since
  ;; I use `shift-left' and `shift-right' bindings in org-mode.
  ;;
  ;; Since my keyboard (HHK) has separate Alt key (apart from Meta
  ;; key), I use the 'alt instead of 'shift -- cinsk
  (windmove-default-keybindings 'alt))

;;;
;;; YAML mode
;;;
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

;;;
;;; CSS mode
;;;
(use-package css-mode
  :mode "\\.css\\'"
  :config
  (setq cssm-indent-function #'cssm-c-style-indenter))

;;;
;;; htmlize
;;;
(use-package htmlize
  :disabled
  :config
  (setq htmlize-convert-nonascii-to-entities nil))


(use-package org-bookmarks
  :ensure nil
  :if (locate-library "org-bookmarks")
  :custom ((org-bookmarks-file "~/agenda/bookmarks.org")
           (org-bookmarks-add-org-capture-template t))
  :commands (org-bookmarks)
  :init (org-bookmarks-add-org-capture-template))

;;;
;;; gnuplot
;;;
(use-package gnuplot
  :if (locate-library "gnuplot")
  :commands (gnuplot-mode gnuplot-make-buffer)
  :mode ("\\.gp\\'" . gnuplot-mode))



;;;
;;; go lang
;;;
(uinit "_go" :when (locate-library "go-mode"))

(use-package go-mode
  :if (locate-library "go-mode")
  :mode "\\.go\\'")


;;;
;;; YASnippet -- http://code.google.com/p/yasnippet/
;;;
(use-package yasnippet
  :ensure t
  :hook (after-init . yas-global-mode)
  :bind ("C-x C-/" . yas-insert-snippet)
  :mode ("/\\.emacs\\.d/snippets/.*/[^./][^/]*\\'" . snippet-mode)
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-prompt-functions '(yas-completing-prompt
                          yas-dropdown-prompt
                          yas-no-prompt)))

(use-package joplin-mode
  :load-path "~/src/joplin-mode"
  :requires markdown-mode
  :config
  (when (wsl-p)
    (let* ((hostip (wsl-host-address))
           (endp (concat hostip ":8080")))
      (setq joplin-curl-args (list "--proxy"
                                   (concat "http://" endp))
            joplin-url-proxy (list (cons "http" endp)))))
  (add-to-list 'markdown-mode-hook 'joplin-note-mode))


;;
;; No bell (beep)
;;
(setq ring-bell-function 'ignore)








(uinit "_ediff"
  ;; Note that some external packages loads 'ediff by themselves, such
  ;; as magit and color-theme.  Since
  ;; `ediff-make-wide-display-function' should be set before loading
  ;; `ediff, ediff customization should be placed in the first
  ;; place. -- cinsk
  :when 'ediff)


;;; Sometimes, Emacs asks for the confirmation of a command such as
;;; killing a buffer.  In that case, user should type "yes" or "no"
;;; directly.
;;;
;;; Below configuration let the user uses "y" or "n" instead of using
;;; longer version.
(defalias 'yes-or-no-p 'y-or-n-p)



;;; Helpers for TAGS manipulation
(setq tags-add-tables 't)          ; do not ask to add new tags table.

;;; Try completes if the line is already indented
(setq tab-always-indent 'complete)


;;; Set up the keyboard so the delete key on both the regular keyboard
;;; and the keypad delete the character under the cursor and to the
;;; right under X, instead of the default, backspace behavior.
;;;
;; (keymap-global-set "<delete>" #'delete-char)
;; (keymap-global-set "<kp-delete>" #'delete-char)



(uinit "_cc-mode" :when 'cc-mode)



;;; Emacs generates a backup file (filename plus "~") whenever a file
;;; the first time it is saved.  Uncomment below line to prevents it.
;;;
;; (setq-default make-backup-files nil)

(when (or window-system (daemonp))
  ;; The reason of (daemonp) check is that sometimes, Emacs as daemon
  ;; start up without GUI, but later the client will be attached with
  ;; UI.
  (menu-bar-mode 1)                    ; -1 to hide, 1 to show
  (tool-bar-mode -1)                   ; -1 to hide, 1 to show
  )

(uinit "korean" :when enable-multibyte-characters)


;;;
;;; Search and replace related configuration
;;;
(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)

(progn
  ;; The default key binding M-% and C-M-% are too difficult to type.
  ;; Since M-# and C-M-# are not used by official bindings, I'll use
  ;; them.
  (keymap-global-set "M-#" #'query-replace)
  (keymap-global-set "C-M-#" #'query-replace-regexp)

  ;; During isearch, M-% and C-M-% will also launching replace job
  ;; with the last search string.  Thus, I'll add M-% and C-M-% for
  ;; the consistence.
  (keymap-set isearch-mode-map "M-#" #'isearch-query-replace)
  (keymap-set isearch-mode-map "C-M-#" #'isearch-query-replace-regexp)

  ;; During isearch, `M-s w' will toggle word search, which is
  ;; difficult to remember, so use `M-W' instead.
  ;;
  ;; Note that `M-w' works as original, `kill-ring-save'.
  (keymap-set isearch-mode-map "M-W" #'isearch-toggle-word)

  (keymap-set isearch-mode-map "M-/" #'isearch-complete)
  )


(uinit "_shell" :when 'shell)

(uinit "buffer-menu" :when 'buffer-menu)


;;; When a user paste clipboard content in Emacs using mouse button 2,
;;; the content will be pasted in the place at mouse click.  Comment
;;; below line for the default behavior (at mouse click).
(setq mouse-yank-at-point t)

;; browse-kill-ring: interactive kill-ring navigation
;; disabled in favor of `helm-show-kill-ring'
;;
;; (when (uinit-require 'browse-kill-ring)
;;   (when (fboundp 'browse-kill-ring)
;;     (keymap-global-set "C-c C-k" #'browse-kill-ring)))


;;; If you are intended BS (backspace) key to work
;;; correctly on some terminals, uncomment one of below s-exp.
;;;                                                 -- cinsk
;;(keymap-global-set "C-?" #'backward-delete-char)
;;(keymap-global-set "C-h" #'backward-delete-char)

(keymap-global-set "C-c c" #'compile)
(use-package dwim-compile
  :if (locate-library "dwim-compile")
  :config
  (keymap-global-set "C-c c" #'dwim-c/compile))

;; See http://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))



(keymap-global-set "C-c C-c" #'comment-region)
(setq comment-empty-lines t)

(keymap-global-set "C-." #'find-tag-other-window) ; C-x o


(keymap-global-set "C-c i" #'indent-region)

(keymap-global-set "<f11>" #'toggle-case-fold-search)

(show-paren-mode 1)                     ; show matching parenthese

;;;
;;; ICE setup
;;;
(add-to-list 'auto-mode-alist '(".*\\.ice$" . java-mode))

;;;
;;; Abbrev mode and autoload settings
;;;
(let ((abb_default "~/.abbrev_defs")
      (abb_correct (file-name-concat user-emacs-directory "abbrev_defs")))
  ;; Prefer "~/.emacs.d/abbrev_defs" to "~/.abbrev_defs"
  (setq abbrev-file-name
        (if (file-readable-p abb_correct)
            abb_correct
          (if (file-readable-p abb_default)
              abb_default
            abb_correct))))


;;;
;;; Switching between buffers using iswitchb/ido
;;;


(when nil
  (unless (package-installed-p 'helm)
    (use-package iswitchb
      :disabled
      :config
      ;; (iswitchb-mode 1)                     ; smart buffer switching mode
      (setq iswitchb-default-method 'maybe-frame)) ; ask to use another frame.

    (use-package ido
      :config
      (ido-mode 1)

      ;;(setq ido-default-buffer-method 'maybe-frame)
      (setq ido-use-filename-at-point 'guess)
      (setq ido-max-directory-size 100000)

      (when (boundp 'ido-file-completion-map)
        ;; The default binding `C-x C-f' is too much for me
        (keymap-set ido-file-completion-map "C-<return>" #'ido-enter-dired)
        (keymap-set ido-file-completion-map "M-<return>" #'ido-enter-dired)))))

;; I tried helm-mode, which looks promising, but it does not seem to
;; have enough short-cut mechanism to boost user's input.  For
;; example, completing q-r-r is not possible for
;; `query-replace-regexp'.  And if there are multiple choices, I need
;; to navigate using up/down shortcut, with careful on-looking to the
;; helm buffer.  It distract me from what I try to do.  Until helm
;; updated, or until I found better way, I'll stick to
;; 'icomplete-mode.
;(icomplete-mode 1)
;(with-eval-after-load "icomplete"
;  (uinit-require 'icomplete+ nil 'noerror))

;; Now I'm using helm.  One minor problem is, whenever I invoke
;; `helm-find-files', sometimes it freezes.  I believe this is related
;; to the garbage collection. See the comment in the beginning of the
;; file for `garbage-collection-messages'.

;; (uinit "_helm" :when (locate-library "helm"))
(uinit "vomce")

;;
;; Normally, switching buffer commands (e.g. `switch-to-buffer' or
;; `iswitchb-buffer') ignore buffer that matched "^ ".
;;
;; When I develop some lisp package, I need to switch to buffers that
;; begins with "^ ".  Thus, I don't use "^ " as the value of
;; `iswitchb-buffer-ignore'.
;;
;; The problem is, sometimes " *Minibuf-1*" buffer will be in the buffer
;; list, which makes me annoying.  Thus, I'll explicitly ignore minibuffer-like
;; names in `iswitchb-buffer-ignore'. -- cinsk
;;
;; (setq iswitchb-buffer-ignore '("^ \\*Minibuf[^*]*\\*"))



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

(setq resize-minibuffer-mode t)         ; ensure all contents of mini
                                        ; buffer visible

;; (when (display-graphic-p)
;;   (global-hl-line-mode 1))


;; I'll try the `ido-use-filename-at-point' instead ffap for now
;;
;;(setq ffap-require-prefix t)
;;(ffap-bindings)                         ; context-sensitive find-file


(uinit "delete" :when 'delete)

;;;
;;; emacs server
;;;
(when (and (not noninteractive) (display-graphic-p))
  (ignore-errors
    (server-start)))

;;;
;;; I prefer case-sensitive search & replace
;;;
(setq-default case-fold-search nil)
(setq-default tags-case-fold-search nil)

(fset 'find-next-tag "\C-u\256")        ; macro for C-u M-.
(fset 'find-prev-tag "\C-u-\256")       ; macro for C-u - M-.

(keymap-global-set "M-]" #'find-next-tag)
(keymap-global-set "M-[" #'find-prev-tag)

;;(keymap-global-set "<up>"   (lambda () (interactive) (scroll-down 1)))
;;(keymap-global-set "<down>" (lambda () (interactive) (scroll-up 1)))

(defun scroll-other-frame (&optional arg)
  "Scroll the selected window in the next frame.
Analogous to `scroll-other-window', but targets the next frame instead
of the next window."
  (interactive "P")
  (let* ((target-frame (next-frame))
         (target-window (frame-selected-window target-frame)))
    (if (window-live-p target-window)
        (with-selected-window target-window
          (scroll-up arg))
      (user-error "No valid window found in the next frame"))))

(defun scroll-other-frame-down (&optional arg)
  "Scroll the selected window in the next frame downwards.
Analogous to `scroll-other-window-down', but targets the next frame
instead of the next window."
  (interactive "P")
  (let* ((target-frame (next-frame))
         (target-window (frame-selected-window target-frame)))
    (if (window-live-p target-window)
        (with-selected-window target-window
          (scroll-down arg))
      (user-error "No valid window found in the next frame"))))

(keymap-global-set "M-S-<prior>" #'scroll-other-frame-down)
(keymap-global-set "M-S-<next>" #'scroll-other-frame)



(keymap-global-set "C-M-]" #'forward-page)
(keymap-global-set "C-M-[" #'backward-page)



(when (and (or (display-graphic-p) (daemonp))
           (eq (lookup-key (current-global-map) [(control ?z)])
               #'suspend-frame))
  ;; `suspend-frame' is bound to `C-z' and `C-x C-z'.
  ;; As I accidentally press `C-z' so remove its binding.
  (keymap-global-unset "C-z"))




(uinit "window-frame" :when 'window-frame)


;;;
;;; vim-modeline
;;;
(let ((vim-modeline-path (expand-file-name
                          (file-name-concat user-emacs-directory
                                            "vim-modeline"))))
  (when (file-accessible-directory-p vim-modeline-path)
    (add-to-list 'load-path vim-modeline-path)
    (when (locate-library "vim-modeline")
      (require 'vim-modeline)
      (add-hook 'find-file-hook 'vim-modeline/do))))


(uinit "vcs" :when 'version-control-system)




(uinit "_paren.el" :when (or (locate-library "paredit")
                             (locate-library "smartparens")))

(uinit "lisp" :when 'lisp)

(uinit "_clojure"
  :when (and (locate-library "clojure-mode") (locate-library "cider-mode")))


(uinit "_latex" :when 'latex-mode)



(uinit "_mmm" :when 'deferred)



(uinit "_nxml" :when (locate-library "nxml-mode"))


(uinit "_dired" :when 'dired)

(keymap-global-set "M-F d <return>" #'find-dired)
(keymap-global-set "M-F g d" #'find-grep-dired)
(keymap-global-set "M-F g <return>" #'find-grep)
(keymap-global-set "M-F r" #'rgrep)


;;;
;;; cscope binding
;;;
;;; You need to install cscope(1) and xcscope.el to use below bindings
;;; Read xcscope.el packaged in cscope source tarball. It can be obtained
;;; from http://cscope.sourceforge.net/
;;;
;;; As xcscope is available in Melpa, manual configuration is no
;;; longer needed.
;; (when (locate-library "xcscope")
;;   (require 'xcscope)
;;   (and (fboundp 'cscope-setup)
;;        (cscope-setup)))

;;;
;;; Version Control
;;;

;; vc-toggle-read-only is an obsolete command (as of 24.1)
(keymap-global-set "C-x C-q"
                   (if (fboundp 'read-only-mode)
                       #'read-only-mode
                     #'vc-toggle-read-only))


;;(split-window-horizontally)


;;(uinit "mail" :when 'mail-news-gnus)
;;(file-exists-p "~/.authinfo.gpg")

(setq send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      smtpmail-smtp-server "smtp.gmail.com")

(setq user-mail-address "cinsky@gmail.com"
      mail-default-headers (format "Comment: sent from emacs-%s@%s\n"
                                   emacs-version
                                   (system-name)))

(when (file-exists-p "~/.emacs.d/secrets/.authinfo.gpg")
  (setq auth-sources
        '((:source "~/.emacs.d/secrets/.authinfo.gpg"))))



;;; GPG passphrase entry: loopback mode
;;
;; Why: Emacs decrypts ~/.authinfo.gpg (used by auth-source, and so by
;; TRAMP, Gnus, smtpmail, etc.) by running `gpg'.  Normally gpg-agent
;; starts an external pinentry program (GTK dialog, curses, pinentry-mac)
;; to ask for the passphrase.  That breaks in terminal Emacs, over SSH,
;; or with no display.
;;
;; Loopback: gpg-agent does NOT start a pinentry.  It sends the request
;; back to `gpg', which sends it back to Emacs (epg).  Emacs then asks
;; in the minibuffer via `read-passwd':
;;
;;   Emacs <-- gpg <-- gpg-agent   ("need passphrase")
;;   Emacs --> gpg --> gpg-agent   (passphrase)
;;
;; gpg-agent still caches the passphrase (see default-cache-ttl /
;; max-cache-ttl in gpg-agent.conf), so you aren't asked every time.
;;
;; No `pinentry' package (pinentry.el) is needed for this.  Don't set
;; INSIDE_EMACS yourself either; epg already sets it for the gpg it starts.
;;
;; ---- One-time GnuPG setup (in case you forgot) ----------------------
;;
;; 1. Allow loopback in ~/.gnupg/gpg-agent.conf (on by default since
;;    GnuPG 2.1.12, but add it explicitly to be safe):
;;
;;      allow-loopback-pinentry
;;
;; 2. Reload the agent so it picks up the change:
;;
;;      gpgconf --reload gpg-agent
;;
;;    (or restart it: gpgconf --kill gpg-agent; the agent starts again
;;    on demand)
;;
;; Check: M-: (epg-decrypt-file (epg-make-context) "~/.authinfo.gpg" nil)
;; should ask for the passphrase in the minibuffer and not in a popup.
;; ----------------------------------------------------------------------

(use-package epg-config
  :ensure nil                      ; built in (Emacs 27+)
  :custom
  (epg-pinentry-mode 'loopback))   ; ask for the GPG passphrase in the minibuffer



;;;
;;; Calender
;;;
(require 'calendar)

(keymap-global-set "C-<f12>" #'calendar)

(let ((my-diary-file (file-name-concat user-emacs-directory "diary")))
  (if (file-readable-p my-diary-file)
      (setq diary-file my-diary-file)))

(setq mark-holidays-in-calendar t)

(when (and (boundp 'diary-file)
           (file-readable-p diary-file))
  (setq mark-diary-entries-in-calendar t))

(add-hook 'diary-display-hook 'fancy-diary-display)

(setq local-holidays
      '((holiday-fixed 11 1 "삼성전자 창립일")))

(let ((cal-korea-path (expand-file-name
                       (file-name-concat user-emacs-directory "cal-korea-x"))))
  (when (file-accessible-directory-p cal-korea-path)
    (add-to-list 'load-path cal-korea-path)
    (when (locate-library "cal-korea-x")
      (require 'cal-korea-x)
      (setq holiday-general-holidays cal-korea-x-korean-holidays))))


(uinit "_org" :when (locate-library "org"))


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
(with-eval-after-load "ispell"
  (setq ispell-dictionary "english"))



;;;
;;; Do not display splash screen on startup
;;;

;; Disable the startup screen
(setq inhibit-splash-screen t)



;; Try to complete the thing at point if the current line is already indented.
(setq tab-always-indent 'complete)

(uinit "ruby" :when (locate-library "ruby-mode"))

(if (locate-library "python-mode")
    ;; remember that Aquamacs provides its own version of python-mode
    ;; which may not the one you're looking for.
    (uinit "_python" :when t)
  (uinit "_py" :when t))


;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/skewer-mode"))
;;(require 'skewer-mode)

(uinit "_js" :when 'js)


(uinit "_rust" :when (locate-library "rust-mode"))


(uinit "maven" :when 'maven)

(uinit "_browse-url" :when (locate-library "browse-url"))



;;;
;;; lua
;;;
(uinit "_lua"
  :when (locate-library "lua-mode")
  :init
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))



(uinit "scala" :when t)



;;;
;;; ESS(Emacs Speaks Statistics) setting for R.
;;;
(uinit "_ess" :when (locate-library "ess-site"))


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
(keymap-global-set "<f10>" #'calc)
(keymap-global-set "C-<f10>" #'quick-calc-dwim)

(defun quick-calc-dwim (&optional arg)
  "Call `calc-eval-region' if there is active region, otherwise
call `quick-calc'"
  (interactive "P")
  (let ((current-prefix-arg arg))
    (if (use-region-p)
        (call-interactively 'calc-eval-region)
      (call-interactively 'quick-calc))))

(defun calc-eval-region (arg beg end)
  "Calculate the region and display the result in the echo area.
With a prefix ARG non-nil, replace the region with the result. With two
prefix ARGs, insert the result at the end of region."
  (interactive "p\nr")
  (let* ((expr (buffer-substring-no-properties beg end))
         (result (calc-eval expr)))
    (cond ((eq arg 4)
           (goto-char beg)
           (push-mark (point) 'nomsg)
           (delete-region beg end)
           (insert result))
          ((eq arg 16)
           (goto-char end)
           (push-mark (point) 'nomsg)
           (insert result))
          (t (message "%s = %s" expr result)))))



(setq tramp-default-method "sshx")

;; This tells TRAMP where to put autosafe files so it has nothing to
;; ask about even if the directory is not writable.
(setq tramp-auto-save-directory
      (expand-file-name "emacs/tramp-autosave/" "~/.cache"))
(unless (file-directory-p tramp-auto-save-directory)
  (make-directory tramp-auto-save-directory t)
  (set-file-modes tramp-auto-save-directory #o700))



;; (let ((dir (expand-file-name "~/.emacs.d/emacs-gradle-mode")))
;;   (when (file-exists-p dir)
;;     (add-to-list 'load-path dir))
;;   (uinit-require 'gradle-mode))



(when (locate-library "projectile")
  (and (fboundp 'projectile-global-mode)
       (projectile-global-mode)))

(with-eval-after-load "graphviz-dot-mode"
  ;; `projectile-mode' uses "C-c p" as a prefix which conflicts with
  ;; `graphviz-dot-preview'.  Since I use `projectile-mode' more
  ;; commonly than `graphviz-dot-mode', I'll bind
  ;; `graphviz-dot-preview to "C-c P"

  (keymap-set graphviz-dot-mode-map "C-c P" #'graphviz-dot-preview))


(defun fkey (arg)
  "Send a function key press event to Emacs.

Without a prefix argument, it simulate <f1> key press.
A prefix argument N is translated to <fn> key"
  (interactive "p")
  (if (<= arg 0)
      (error "prefix argument between 1 to 12 is required"))
  (let ((ks (kbd (format "<f%d>" arg))))
         ;(vector (list (intern (format "f%d" arg))))))
    (message "key: %S" ks)
    (add-to-list 'unread-command-events (listify-key-sequence ks))))

;;(keymap-global-set "<f2>" #'ff-find-other-file)
;;(keymap-global-set "<f3>" #'dired-jump)


;; On Emacs version 25, especiall on text terminal, the key `M-['
;; conflict with other control sequences (e.g. cannot use 'paste the
;; clipboard text' feature of terminal to Emacs.
;;
;; See https://emacs.stackexchange.com/questions/28851/how-to-turn-off-bracketed-paste-mode
(unless window-system
  (keymap-global-unset "M-["))


;; Run `M-x uinit-report' (or see `emacs-init-time') for a breakdown
;; of what got loaded above and how long it took.


;;
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;;

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold cinsk/gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook 'append)


(eval-after-load "magit"
  ;; `magit-commit-reshelve' and `magit-reshelve-since' are available
  ;; only if transient-default-level is equal or higher than 6.
  ;;
  ;; See https://emacs.stackexchange.com/questions/58283/how-do-i-modify-magits-behavior-to-use-the-date-that-i-finalize-the-commit-and-n
  (setq transient-default-level 6))

(uinit "_claude" :when (executable-find "claude"))


;;; ====================================================================
;;; System Specific Configuration
;;; ====================================================================
(uinit "wsl" :when (wsl-p))

(uinit "darwin" :when (eq system-type 'darwin))

(uinit "X" :when (eq window-system 'x))



(load (file-name-concat user-emacs-directory "postinit.el") t)


;;; Local Variables:
;;; coding: utf-8
;;; End:
