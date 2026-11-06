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

;;; ==================================================================
;;; Base Initialization Configuration (directories, mandatory packages)
;;; ==================================================================


;;; Load custom configuration -- Emacs's customizable variables will
;;; update this file if `custom-file' is nil which is not preferrable.
(setq custom-file "~/.emacs.d/my-custom.el")
(load custom-file t)

;;; emacs packages for my personal uses are placed in $HOME/.emacs.d
(setq user-emacs-directory "~/.emacs.d/")

(when (and (not (file-accessible-directory-p user-emacs-directory))
           (not noninteractive))
  (if (yes-or-no-p
       (format "create user directory(%s)? " user-emacs-directory))
      (make-directory user-emacs-directory t)))

(setq initial-buffer-choice "~/.emacs.d/README.org")


;;; ---- package system initialization ----

(require 'package)

;; Keep GNU ELPA and NonGNU ELPA (Emacs defaults), and add MELPA.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Higher number wins.  Prefer official ELPAs, then MELPA Stable,
;; and fall back to MELPA (unstable) only when nothing else has the package.
(setq package-archive-priorities
      '(("gnu"          . 20)
        ("nongnu"       . 15)
        ("melpa-stable" . 10)
        ("melpa"        . 0)))


;; package.el initializes itself before init.el on Emacs 27+, so no
;; explicit (package-initialize) is needed.  If you set
;; `package-enable-at-startup' to nil in early-init.el, add it back here.

;; Bootstrap use-package (built in since Emacs 29).
(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

(require 'use-package)

;; Install missing packages automatically, so you don't need
;; `:ensure t' on every form.
;; (setq use-package-always-ensure t)

;; Collect load-time stats; view with M-x use-package-report.
(setq use-package-compute-statistics t)

;; I will install packages that is not managed by packages.el in
;; "$HOME/.emacs.d/local".
;;
;; Note that packages in this directory has higher priority than others.
(defvar user-custom-packages-directory
  (file-name-concat user-emacs-directory "local")
  "Manually installed packages are in this directory")

(let ((srcpath (expand-file-name (file-name-concat user-emacs-directory "src"))))
  (add-to-list 'load-path srcpath))



(use-package cinsk-common
  :config
  (keymap-global-set "C-c n" #'my/line-numbers-on-region)
  (keymap-global-set "C-c q" #'my/fill-text-line-paragraph)
  (keymap-global-set "M-Q" #'my/unfill-paragraph)
  ;;(my/add-site-lisp-directories "/usr/local/share/emacs/site-lisp")
  ;;(my/add-site-lisp-directories user-custom-packages-directory)
  )


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

(setq uinit-init-directory "~/.emacs.d/init"
      uinit-byte-compile-on-load nil)

;; (byte-recompile-directory package-user-dir nil 'force)
(require 'uinit)



;;
;; Simple configuration for
;;
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq tags-add-tables 't)          ; do not ask to add new tags table.
(setq tab-always-indent 'complete) ; try completes if the line is already indented
(setq mouse-yank-at-point t)	   ; yanking at point not at click position

(setq resize-minibuffer-mode t)	; ensure all contents of minibuffer visible
(setq inhibit-splash-screen t)		; Disable the startup screen
;; (setq-default make-backup-files nil)

(setq-default case-fold-search nil)
(setq-default tags-case-fold-search nil)

(keymap-global-set "C-x C-q" #'read-only-mode)


(put 'narrow-to-region 'disabled nil)	; enable narrow-to-region

(setq comment-empty-lines t)
(keymap-global-set "C-c C-c" #'comment-region)

(when (or window-system (daemonp))
  ;; The reason of (daemonp) check is that sometimes, Emacs as daemon
  ;; start up without GUI, but later the client will be attached with
  ;; UI.
  (menu-bar-mode 1)                    ; -1 to hide, 1 to show
  (tool-bar-mode -1)                   ; -1 to hide, 1 to show
  )

;;;
;;; Text/Programming mode specific
;;;
(defun my/whitespaces-setup ()
  "Whitespace and final-newline settings for programming buffers."
  (setq-local require-final-newline t)
  (setq-local delete-trailing-lines nil)
  ;; (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(dolist (hook '(prog-mode-hook org-mode-hook))
  (add-hook hook #'my/whitespaces-setup))


(progn
  ;; Search and replace related configuration
  (defalias 'qr 'query-replace)
  (defalias 'qrr 'query-replace-regexp)

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

(keymap-global-set "C-." #'find-tag-other-window) ; C-x o
(keymap-global-set "C-c i" #'indent-region)
(keymap-global-set "<f11>" #'toggle-case-fold-search)
(show-paren-mode 1)                     ; show matching parenthese

;;(when window-system
;;  (which-function-mode 1))          ; display function names in mode-line

(global-font-lock-mode 1)           ; every buffer uses font-lock-mode
(line-number-mode 1)                ; show line number in mode-line
(column-number-mode 1)              ; show column number in mode-line


;;;
;;; emacs server
;;;
(when (and (not noninteractive) (display-graphic-p))
  (ignore-errors
    (server-start)))



;;; ==================================================================
;;; Packages Configuration
;;; ==================================================================

(use-package enable-mmode)

(uinit "korean" :when enable-multibyte-characters)

(use-package capitalize+
  :config
  (substitute-key-definition 'capitalize-word 'capitalize-word+
                             (current-global-map)))

;; Abbrev mode and autoload settings
(let ((abb_default "~/.abbrev_defs")
      (abb_correct (file-name-concat user-emacs-directory "abbrev_defs")))
  ;; Prefer "~/.emacs.d/abbrev_defs" to "~/.abbrev_defs"
  (setq abbrev-file-name
        (if (file-readable-p abb_correct)
            abb_correct
          (if (file-readable-p abb_default)
              abb_default
            abb_correct))))

(use-package xskel
  :init
  (setq auto-insert-query nil))

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

(use-package untabify
  :disabled
  :config
  (setq-default indent-tabs-mode nil)     ; do not insert tab character.
  (add-hook 'before-save-hook 'untabify-remove-trailing-spaces-on-write))


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

(when (and (or (display-graphic-p) (daemonp))
           (eq (lookup-key (current-global-map) [(control ?z)])
               #'suspend-frame))
  ;; `suspend-frame' is bound to `C-z' and `C-x C-z'.
  ;; As I accidentally press `C-z' so remove its binding.
  (keymap-global-unset "C-z"))

(uinit "window-frame" :when 'window-frame)

(uinit "_ediff"
  ;; Note that some external packages loads 'ediff by themselves, such
  ;; as magit and color-theme.  Since
  ;; `ediff-make-wide-display-function' should be set before loading
  ;; `ediff, ediff customization should be placed in the first
  ;; place. -- cinsk
  :when 'ediff)



(uinit "buffer-menu" :when 'buffer-menu)


(uinit "_shell" :when 'shell)


(use-package joplin-mode
  :load-path "~/src/joplin-mode"
  :if (locate-library "joplin-mode")
  :requires markdown-mode
  :config
  (when (my/wsl-p)
    (let* ((hostip (wsl-host-address))
           (endp (concat hostip ":8080")))
      (setq joplin-curl-args (list "--proxy"
                                   (concat "http://" endp))
            joplin-url-proxy (list (cons "http" endp)))))
  (add-to-list 'markdown-mode-hook 'joplin-note-mode))


;; (uinit "_helm" :when (locate-library "helm"))
(uinit "vomce")


(uinit "delete" :when 'delete)

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



(uinit "_paren.el" :when (or (locate-library "paredit")
                             (locate-library "smartparens")))

(uinit "_mmm" :when 'deferred)

(uinit "_dired" :when 'dired)

(when (executable-find "rg")
  (use-package rg
    :ensure t
    :init
    (keymap-global-set "C-c s" #'rg-menu)))


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
(fset 'find-next-tag "\C-u\256")        ; macro for C-u M-.
(fset 'find-prev-tag "\C-u-\256")       ; macro for C-u - M-.

(keymap-global-set "M-]" #'find-next-tag)
(keymap-global-set "M-[" #'find-prev-tag)


;;(uinit "mail" :when 'mail-news-gnus)

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


(setq tramp-default-method "sshx")

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


(uinit "_org" :when (locate-library "org"))
(uinit "_calendar")                     ; after _org: uses `org-directory'

(use-package org-bookmarks
  :ensure nil
  :if (locate-library "org-bookmarks")
  :custom ((org-bookmarks-file "~/agenda/bookmarks.org")
           (org-bookmarks-add-org-capture-template t))
  :commands (org-bookmarks)
  :init (org-bookmarks-add-org-capture-template))

(use-package yaml-mode
  :if (locate-library "yaml-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

(use-package css-mode
  :mode "\\.css\\'"
  :config
  (setq cssm-indent-function #'cssm-c-style-indenter))

(with-eval-after-load "graphviz-dot-mode"
  ;; `projectile-mode' uses "C-c p" as a prefix which conflicts with
  ;; `graphviz-dot-preview'.  Since I use `projectile-mode' more
  ;; commonly than `graphviz-dot-mode', I'll bind
  ;; `graphviz-dot-preview to "C-c P"

  (keymap-set graphviz-dot-mode-map "C-c P" #'graphviz-dot-preview))

;;;
;;; ispell(aspell) configuration
;;;
;;; Currently neither of them provides Korean dictionary.
;;; Currently, ispell complained that it does not have proper dictionary in
;;; Korean language environment.
(with-eval-after-load "ispell"
  (setq ispell-dictionary "english"))


(use-package cmake-mode
  :disabled
  :config
  (setq auto-mode-alist
        (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                  ("\\.cmake\\'" . cmake-mode))
                auto-mode-alist)))

(uinit "maven" :when 'maven)


;;; Programming Language

(uinit "_cc-mode" :when 'cc-mode)

(uinit "_go" :when (locate-library "go-mode"))

(use-package go-mode
  :if (locate-library "go-mode")
  :mode "\\.go\\'")

(use-package gnuplot
  :if (locate-library "gnuplot")
  :commands (gnuplot-mode gnuplot-make-buffer)
  :mode ("\\.gp\\'" . gnuplot-mode))

(uinit "lisp" :when 'lisp)

(uinit "_clojure"
  :when (and (locate-library "clojure-mode") (locate-library "cider-mode")))

(uinit "_latex" :when 'latex-mode)

(uinit "_nxml" :when (locate-library "nxml-mode"))

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

(uinit "_lua"
  :when (locate-library "lua-mode")
  :init
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))

(uinit "scala" :when t)

(uinit "_ess" :when (locate-library "ess-site")) ; statistics/R


(uinit "_calc")


(keymap-global-set "C-c c" #'compile)
(use-package dwim-compile
  :if (locate-library "dwim-compile")
  :config
  (keymap-global-set "C-c c" #'dwim-c/compile))


(when (locate-library "projectile")
  (and (fboundp 'projectile-global-mode)
       (projectile-global-mode)))



(uinit "vcs" :when 'version-control-system)

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

;; If you are intended BS (backspace) key to work
;; correctly on some terminals, uncomment one of below s-exp.
;; (keymap-global-set "C-?" #'backward-delete-char)
;; (keymap-global-set "C-h" #'backward-delete-char)

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the
;; right under X, instead of the default, backspace behavior.
;; (keymap-global-set "<delete>" #'delete-char)
;; (keymap-global-set "<kp-delete>" #'delete-char)

;; On Emacs version 25, especiall on text terminal, the key `M-['
;; conflict with other control sequences (e.g. cannot use 'paste the
;; clipboard text' feature of terminal to Emacs.
;;
;; See https://emacs.stackexchange.com/questions/28851/how-to-turn-off-bracketed-paste-mode

(unless window-system
  (keymap-global-unset "M-["))

(use-package fontutil
  :config
  ;; FYI, changing UI components (fonts, colors), place the config in postinit.el
  (fontutil/install-mouse-wheel))

(uinit "_browse-url" :when (locate-library "browse-url"))
(uinit "wsl" :when (my/wsl-p))
(uinit "darwin" :when (eq system-type 'darwin))
(uinit "X" :when (eq window-system 'x))



(load (file-name-concat user-emacs-directory "postinit.el") t)


;;; Local Variables:
;;; coding: utf-8
;;; End:
