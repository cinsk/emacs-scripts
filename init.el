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
;;; $ git clone http://github.com/cinsk/emacs-scripts.git .emacs.d
;;;

;;; The default value of `gc-cons-threshold' is set to 800000 on
;;; 64-bit system.

(defvar cinsk/gc-cons-threshold (* 10 gc-cons-threshold))
(setq gc-cons-threshold cinsk/gc-cons-threshold)
;;(setq garbage-collection-messages t)

;;;
;;; emacs packages for my personal uses are placed in $HOME/.emacs.d
;;;
(require 'cl)

(defun path-join (path &rest args)
  "Join all parameters to build a pathname."
  (if args
      (apply 'path-join
             (concat (file-name-as-directory path) (car args))
             (cdr args))
    path))

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

(defmacro enable-minor-mode (major-mode-file major-mode-hook feature minor-mode &rest condition)
  "Enable MINOR-MODE on a major mode based on the CONDITION.

This macro will register a function which enables MINOR-MODE
based on CONDITION to the hook, MAJOR-MODE-HOOK.  Since
MAJOR-MODE-HOOK might not available by the time this macro
called, the whole code will be body of `with-eval-after-load'
using MAJOR-MODE-FILE.

If CONDITION is omitted, MODE will be enabled always. If
CONDITION is a string, it will be treated as a regular expression
pattern and if it matches to the buffer-file-name, MODE will be
enabled.  Otherwise, CONDITION is treated as a form and MODE will
be enabled if CONDITION evaluated as true.

Examples:

  (enable-minor-mode \"js2-mode\" js2-mode-hook skewer-mode skewer-mode)
  (enable-minor-mode \"js2-mode\" js2-mode-hook skewer-mode skewer-mode \"/d3/.*\\\\.js\\\\'\")
  (enable-minor-mode \"js2-mode\" js2-mode-hook skewer-mode skewer-mode
  ;; ...
  t)
"
  (declare (indent 4))
  `(when (locate-library ,major-mode-file)
     (with-eval-after-load ,major-mode-file
       (require (quote ,feature))
       (add-to-list (quote ,major-mode-hook)
                    (lambda ()
                      (when ,(cond ((null condition) t)
                                   ((and (eq (length condition) 1) (stringp (car condition))) `(string-match ,@condition (buffer-file-name nil)))
                                   (t `(progn ,@condition)))
                        (,minor-mode)))))))



(when (string-equal (getenv "SHELL") "/bin/false")
  ;; "$SHELL" is set to "/bin/false" in darwin under Amazon User
  ;; Cert. System. -- Hmm. this seems not true.
;;  (let ((sh (or (executable-find "bash") "/bin/sh")))
  (let ((sh (or (executable-find "zsh") "/bin/zsh")))
    (setenv "SHELL" sh)
    (setq shell-file-name sh)))



(setq user-emacs-directory "~/.emacs.d/")

(when (and (not (file-accessible-directory-p user-emacs-directory))
           (not noninteractive))
  (if (yes-or-no-p
       (format "create user directory(%s)? " user-emacs-directory))
      (make-directory user-emacs-directory t)))



;;;
;;; package
;;;

;;;
;;; Whenever major upgrade for Emacs, try to execute below sexp to recompile
;;; everything in elpa/ directory.
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

(let ((pkgdir (path-join user-emacs-directory "package")))
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
    ;; According to the package.el, if `package-enable-at-startup' is
    ;; t, it will load all the packages on start up.  But it didn't.
    ;; Perhaps it's a problem related to the version (currently Emacs
    ;; 23).  Thus, I'll force to load it here.
    (package-initialize)
    (setq package-enable-at-startup t)))


;;(unless (locate-library "s")
;;  (package-install 's))

;(add-to-list 'load-path (expand-file-name "~/.emacs.d/skewer-mode"))
;(require 'skewer-mode)


;; I will install packages that is not managed by packages.el in
;; "$HOME/.emacs.d/site-lisp".
;;
;; Note that packages in this directory has higher priority than others.

(defvar user-custom-packages-directory
  (path-join user-emacs-directory "local")
  "Manually installed packages are in this directory")


;;
;; No bell (beep)
;;
(setq ring-bell-function 'ignore)



(let ((srcpath (expand-file-name (path-join user-emacs-directory "src"))))
  (add-to-list 'load-path srcpath)
  )

(setq uinit/init-directory "~/.emacs.d/init"
      uinit/use-byte-compile t)

;; (byte-recompile-directory package-user-dir nil 'force)

(require 'uinit)
(uinit/require 'cinsk-common)

(global-set-key [(control ?c) ?n] #'cinsk/line-numbers-on-region)
(global-set-key [(control ?c) ?q] 'fill-text-line-paragraph)

(cinsk/add-site-lisp-directories "/usr/local/share/emacs/site-lisp")
(cinsk/add-site-lisp-directories user-custom-packages-directory)


(when (file-directory-p "/apollo/env/EmacsAmazonLibs/share/emacs/site-lisp")
  (add-to-list 'load-path "/apollo/env/EmacsAmazonLibs/share/emacs/site-lisp"))

(when (locate-library "amz-common")
  (uinit/require 'amz-common)

  (when (boundp 'c-default-style)
    ;; Amazon library uses BSD style for C/C++, which I don't like
    (setq c-default-style (delq (assoc 'c++-mode c-default-style)
                                c-default-style)
          c-default-style (delq (assoc 'c-mode c-default-style)
                                c-default-style))))


(uinit/require 'capitalize+)
(substitute-key-definition 'capitalize-word 'capitalize-word+
                           (current-global-map))


(uinit/load "darwin"
  (eq system-type 'darwin))


(uinit/load "X"
  (eq window-system 'x))



(uinit/load "_ediff"
  ;; Note that some external packages loads 'ediff by themselves, such
  ;; as magit and color-theme.  Since
  ;; `ediff-make-wide-display-function' should be set before loading
  ;; `ediff, ediff customization should be placed in the first
  ;; place. -- cinsk
  'ediff)


(when (uinit/require 'fontutil)
  ;; For the actual fontset selection, see init/X.el or init/darwim.el
  (fontutil/install-mouse-wheel))



;;; Sometimes, Emacs asks for the confirmation of a command such as
;;; killing a buffer.  In that case, user should type "yes" or "no"
;;; directly.
;;;
;;; Below configuration let the user uses "y" or "n" instead of using
;;; longer version.
(defalias 'yes-or-no-p 'y-or-n-p)



;;; Helpers for TAGS manipulation
(setq tags-add-tables 't)               ; do not ask to add new tags table.


;;; Set up the keyboard so the delete key on both the regular keyboard
;;; and the keypad delete the character under the cursor and to the right
;;; under X, instead of the default, backspace behavior.
;;;
;; (global-set-key [delete] 'delete-char)
;; (global-set-key [kp-delete] 'delete-char)



(uinit/load "_cc-mode"
  'cc-mode)



;;; Emacs generates a backup file (filename plus "~") whenever a file
;;; the first time it is saved.  Uncomment below line to prevents it.
;;;
;; (setq-default make-backup-files nil)

;;;
;;; Window-less system Configuration
;;;
(when (or window-system (daemonp))
  (menu-bar-mode 1)                    ; -1 to hide, 1 to show
  (tool-bar-mode -1)                   ; -1 to hide, 1 to show
  )

(uinit/load "korean"
  enable-multibyte-characters)


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



(uinit/load "_shell"
  'shell)

(uinit/load "buffer-menu"
  'buffer-menu)


;;; When a user paste clipboard content in Emacs using mouse button 2,
;;; the content will be pasted in the place at mouse click.  Comment
;;; below line for the default behavior (at mouse click).
(setq mouse-yank-at-point t)

;; browse-kill-ring: interactive kill-ring navigation
;; disabled in favor of `helm-show-kill-ring'
;;
;; (when (uinit/require 'browse-kill-ring)
;;   (when (fboundp 'browse-kill-ring)
;;     (global-set-key [(control ?c) (control ?k)] 'browse-kill-ring)))


;;;
;;; If you are intended BS (backspace) key to work
;;; correctly on some terminals, uncomment one of below s-exp.
;;;                                                 -- cinsk
;;(global-set-key [C-?] 'backward-delete-char)
;;(global-set-key [C-h] 'backward-delete-char)

(global-set-key "\C-cc" 'compile)
(when (uinit/require 'dwim-compile)
  (require 'dwim-compile)
  (global-set-key [(control ?c) ?c] 'dwim-c/compile))



(global-set-key [(control ?c) (control ?c)] 'comment-region)
(setq comment-empty-lines t)

(global-set-key [?\C-.] 'find-tag-other-window) ; C-x o


(global-set-key [(control c) ?i] 'indent-region)

(global-set-key [(f11)] 'toggle-case-fold-search)

;;; Show matching parenthese
(when (fboundp 'show-paren-mode)
  (show-paren-mode 1))

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
      (abb_correct (path-join user-emacs-directory "abbrev_defs")))
  ;; Prefer "~/.emacs.d/abbrev_defs" to "~/.abbrev_defs"
  (setq abbrev-file-name
        (if (file-readable-p abb_correct)
            abb_correct
          (if (file-readable-p abb_default)
              abb_default
            abb_correct))))


;;;
;;; Use hippie expansion for dynamic abbreviation
;;;
(when (uinit/require 'hippie-exp)
  (global-set-key [(meta ?/)] 'hippie-expand)
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

;;;
;;; Switching between buffers using iswitchb/ido
;;;

(unless (locate-library "ido")
  (iswitchb-mode 1)                     ; smart buffer switching mode
  (setq iswitchb-default-method 'maybe-frame)) ; ask to use another frame.

(when (uinit/require 'ido)
  (ido-mode 1)

  ;;(setq ido-default-buffer-method 'maybe-frame)
  (setq ido-use-filename-at-point 'guess)
  (setq ido-max-directory-size 100000)

  (when (boundp 'ido-file-completion-map)
    ;; The default binding `C-x C-f' is too much for me
    (define-key ido-file-completion-map [(control return)] 'ido-enter-dired)
    (define-key ido-file-completion-map [(meta return)] 'ido-enter-dired)))

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


;; I tried helm-mode, which looks promising, but it does not seem to
;; have enough short-cut mechanism to boost user's input.  For
;; example, completing q-r-r is not possible for
;; `query-replace-regexp'.  And if there are multiple choices, I need
;; to navigate using up/down shortcut, with careful on-looking to the
;; helm buffer.  It distract me from what I try to do.  Until helm
;; updated, or until I found better way, I'll stick to
;; 'icomplete-mode.
(icomplete-mode 1)
(with-eval-after-load "icomplete"
  (uinit/require 'icomplete+ nil 'noerror))

(when (> emacs-major-version 23)
  ;; diable completion cycling
  (setq completion-cycle-threshold nil))

;; Now I'm using helm.  One minor problem is, whenever I invoke
;; `helm-find-files', sometimes it freezes.  I believe this is related
;; to the garbage collection. See the comment in the beginning of the
;; file for `garbage-collection-messages'.
(uinit/load "_helm" (locate-library "helm"))



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


(uinit/require 'untabify)




(uinit/load "delete"
  'delete)

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



(when (and (or (display-graphic-p) (daemonp))
           (eq (lookup-key (current-global-map) [(control ?z)])
               #'suspend-frame))
  ;; `suspend-frame' is bound to `C-z' and `C-x C-z'.
  ;; As I accidentally press `C-z' so remove its binding.
  (global-unset-key [(control ?z)]))




(uinit/load "window-frame"
                    'window-frame)

;;;
;;; Markdown mode
;;;
(with-eval-after-load "markdown-mode"
  (add-to-list 'markdown-css-paths "http://thomasf.github.io/solarized-css/solarized-light.min.css"))


;;;
;;; vim-modeline
;;;
(let ((vim-modeline-path (expand-file-name
                          (path-join user-emacs-directory "vim-modeline"))))
  (when (file-accessible-directory-p vim-modeline-path)
    (add-to-list 'load-path vim-modeline-path)
    (when (locate-library "vim-modeline")
      (require 'vim-modeline)
      (add-hook 'find-file-hook 'vim-modeline/do))))


(uinit/load "vcs"
  'version-control-system)


(when (locate-library "cmake-mode")
  (require 'cmake-mode)
  (setq auto-mode-alist
        (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                  ("\\.cmake\\'" . cmake-mode))
                auto-mode-alist)))


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




;;
;; Company mode
;;
(with-eval-after-load "company"
  (define-key company-mode-map [(control meta ?i)] 'company-complete-common)
  (define-key company-active-map [(control ?n)] 'company-select-next)
  (define-key company-active-map [(control ?p)] 'company-select-previous)
  (define-key company-active-map [(control ?v)] 'company-next-page)
  (define-key company-active-map [(meta ?v)] 'company-previous-page)
  )


;;(require 'autofit-frame)
;;(add-hook 'after-make-frame-functions 'fit-frame)
;;
;;(add-hook 'temp-buffer-show-hook
;;          'fit-frame-if-one-window 'append)
(when (locate-library "rainbow-delimiters")
  (mapc (lambda (hook)
          (add-hook hook 'rainbow-delimiters-mode))
        (list 'emacs-lisp-mode-hook
              'lisp-interaction-mode-hook
              'ielm-mode-hook
              'racket-mode-hook
              'scheme-mode-hook
              'clojure-mode-hook)))

(uinit/load "_paren.el" (or (locate-library "paredit")
                            (locate-library "smartparens")))

(uinit/load "lisp"
  'lisp)

(uinit/load "_clojure"
  (and (locate-library "clojure-mode") (locate-library "cider-mode")))


(uinit/load "_latex"
  'latex-mode)



(uinit/load "_mmm" 'deferred)



(uinit/load "_nxml"
  (or (locate-library "nxml-mode")
      ;; For legacy nxml-mode which does not use `provide' for nxml-mode.
      (locate-library "rng-auto")))


(uinit/load "dired"
  'dired)

(global-set-key [(meta ?F) ?d return] 'find-dired)
(global-set-key [(meta ?F) ?g ?d] 'find-grep-dired)
(global-set-key [(meta ?F) ?g return] 'find-grep)
(global-set-key [(meta ?F) ?r] 'rgrep)


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
(global-set-key [(control x) (control q)]
                (if (fboundp 'read-only-mode)
                    'read-only-mode
                  'vc-toggle-read-only))


;;(split-window-horizontally)

(uinit/load "mail"
  'mail-news-gnus)


;;(uinit/load "color" 'color-theme)


;;;
;;; YAML mode
;;;
(when (locate-library "yaml-mode")
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))


;;;
;;; CSS mode
;;;
(with-eval-after-load "css-mode"
  (setq cssm-indent-function #'cssm-c-style-indenter))
(autoload 'css-mode "css-mode" "CSS editing major mode" t)
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))


;;;
;;; htmlize
;;;
(when (locate-library "htmlize")
  (require 'htmlize)
  (setq htmlize-convert-nonascii-to-entities nil))


;;;
;;; Calender
;;;
(require 'calendar)

(global-set-key [(control f12)] 'calendar)

(let ((my-diary-file (path-join user-emacs-directory "diary")))
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
                       (path-join user-emacs-directory "cal-korea-x"))))
  (when (file-accessible-directory-p cal-korea-path)
    (add-to-list 'load-path cal-korea-path)
    (when (locate-library "cal-korea-x")
      (require 'cal-korea-x)
      (setq holiday-general-holidays cal-korea-x-korean-holidays))))


(when (locate-library "amazon-util")
  (require 'amazon-util))


(uinit/load "_org" (locate-library "org"))


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
  (with-eval-after-load "erc"
    (setq erc-default-coding-system '(cp949 . undecided))
    (setq erc-nick '("cinsk" "cinsky" "cinsk_" "cinsk__"))
    (setq erc-user-full-name "Seong-Kook Shin")
    (setq erc-server "localhost:8668")))


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



(uinit/load "ruby"
  (locate-library "ruby-mode"))

(uinit/load "_python"
  ;; remember that Aquamacs provides its own version of python-mode
  ;; which may not the one you're looking for.
  (locate-library "python-mode"))


(uinit/load "_js" 'js)


(uinit/load "_rust"
  (locate-library "rust-mode"))


(uinit/load "maven" 'maven)


;;;
;;; w3m
;;;

;; Nov. 24 2013,  In Mac
;; The current emacs-w3m-1.4.4.tar.gz does not support emacs version 24.
;; unpack the source, do the following:
;;
;;  $ autoconf
;;  $ ./configure --prefix=SOMEWHERE
;;  $ make all
;;  $ make install
;;  $ cp SOMEWHERE/share/emacs/site-lisp/w3m/* ~/.emacs.d/w3m
;;  # There's some problem to load byte compiled w3m package
;;  $ rm ~/.emacs.d/w3m/*.elc
;;  # Search all .el files for widget-mouse-face and comment the line
;;  # that make widget-mouse-face buffer local.

(let ((w3mdir (expand-file-name (path-join user-emacs-directory "w3m"))))
  (when (file-directory-p w3mdir)
    (add-to-list 'load-path w3mdir)
    (add-to-list 'Info-directory-list w3mdir)))

(when (locate-library "w3m")
  (setq w3m-use-cookies t)
  (when (locate-library "w3m-load")
    ;; w3m-load is not available if w3m is installed via packages
    (require 'w3m-load))

  ;; If you have an error message like:
  ;; "w3m-toolbar-define-keys: `keymap' is reserved for embedded parent maps"
  ;;
  ;; set `w3m-use-toolbar' to nil might help.
  ;;
  ;; Reference: http://emacs-w3m.namazu.org/ml/msg11491.html

  (setq w3m-use-toolbar nil)
)



(uinit/load "_browse-url"
  (locate-library "browse-url"))


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
(uinit/load "_go" (locate-library "go-mode"))

(unless (locate-library "go-mode")
  (let* ((godir "/usr/local/go")
         (gldir (path-join godir "misc/emacs")))
    (when (file-accessible-directory-p gldir)
      (add-to-list 'load-path gldir 'append)
      (when (locate-library "go-mode-load")
        (require 'go-mode-load)))))



;;;
;;; lua
;;;
(uinit/load "_lua"
  (when (locate-library "lua-mode")
    (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
    (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))))


;;;
;;; YASnippet -- http://code.google.com/p/yasnippet/
;;;
(when (locate-library "yasnippet")
  ;; tested with yasnippet 0.8.0 (beta)

  (require 'yasnippet)

  ;; The official document describes the snippets directory in
  ;; "~/.emacs.d/plugins/yasnippet-x.y.z/snippets", whereas Gentoo
  ;; yasnippet package installed them in
  ;; "/usr/share/emacs/etc/yasnippet/snippets".
  (setq yas-snippet-dirs (cinsk/accessible-directories
                          "~/.emacs.d/snippets"))

                          ;; (concat (file-name-directory
                          ;;          (locate-library "yasnippet"))
                          ;;         "snippets")
                          ;; "/usr/share/emacs/etc/yasnippet/snippets"))

  (yas-global-mode 1)

  (global-set-key [(control ?x) (control ?/)] 'yas-insert-snippet)

  (setq yas/prompt-functions '(yas-ido-prompt
                               yas-dropdown-prompt
                               yas-completing-prompt
                               yas-no-prompt))
  (unless (eq system-type 'darwin)
    (add-to-list 'yas/prompt-functions 'yas-x-prompt))

  (defun apply-snippet-mode ()
    "Change major-mode to `snippet-mode' if the current file is
in any of directory in `yas-snippet-dirs'."
    (when (buffer-file-name)
      (let ((fullname (expand-file-name (buffer-file-name)))
            (filename (file-name-nondirectory (buffer-file-name))))
        (when (and (not (string-prefix-p "." filename))
                   (< 0 (apply '+
                               (mapcar (lambda (dir)
                                         (let ((absdir (expand-file-name dir)))
                                           (if (string-prefix-p absdir fullname)
                                               1
                                             0)))
                                       yas-snippet-dirs))))
          (snippet-mode)))))

  (add-to-list 'find-file-hook 'apply-snippet-mode)

  (when nil
    ;; While old version (e.g. 0.6.1b) uses `yas/root-directory', and
    ;; provides `yas/reload-all', new version uses `yas-snippets-dirs'
    ;; and provides `yas-reload-all'.
    (if (not (fboundp 'yas-reload-all))
        (progn
          (setq yas/root-directory yas-snippet-dirs)
          (and (fboundp 'yas/reload-all)
               (yas/reload-all)))
      ;; Otherwise, use the new interface.
      (and (fboundp 'yas-reload-all)
           (yas-reload-all))))


  )


(uinit/load "scala" t)



;;;
;;; ESS(Emacs Speaks Statistics) setting for R.
;;;
(uinit/load "_ess"
  (locate-library "ess-site"))


(uinit/load "_sunrise"
  (locate-library "sunrise-commander"))


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
(global-set-key [(control f10)] 'quick-calc-dwim)

(defun quick-calc-dwim (arg)
  "Call `calc-eval-region' if there is active region, otherwise
call `quick-calc'"
  (interactive "P")
  (let ((prefix (prefix-numeric-value arg)))
    (let ((current-prefix-arg prefix))
      (if (use-region-p)
          (call-interactively 'calc-eval-region)
        (call-interactively 'quick-calc)))))

(defun calc-eval-region (arg beg end)
  "Calculate the region and display the result in the echo area.
With a prefix ARG non-nil, replace the region with the result. With two prefix ARGs, insert the result at the end of region."
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


(when (locate-library "eimp")
  (add-hook 'image-mode-hook 'eimp-mode))



(let ((dir (expand-file-name (path-join user-emacs-directory "backups"))))
  (when (file-directory-p dir)
    (add-to-list 'tramp-backup-directory-alist (cons "." dir))))



;; (let ((dir (expand-file-name "~/.emacs.d/emacs-gradle-mode")))
;;   (when (file-exists-p dir)
;;     (add-to-list 'load-path dir))
;;   (uinit/require 'gradle-mode))



(when (locate-library "dtrace-script-mode")
  (autoload 'dtrace-script-mode "dtrace-script-mode" () t)
  (add-to-list 'auto-mode-alist '("\\.d\\'" . dtrace-script-mode)))



(when (locate-library "projectile")
  (and (fboundp 'projectile-global-mode)
       (projectile-global-mode)))

(with-eval-after-load "graphviz-dot-mode"
  ;; `projectile-mode' uses "C-c p" as a prefix which conflicts with
  ;; `graphviz-dot-preview'.  Since I use `projectile-mode' more
  ;; commonly than `graphviz-dot-mode', I'll bind
  ;; `graphviz-dot-preview to "C-c P"

  (define-key graphviz-dot-mode-map [(control ?c) ?P] 'graphviz-dot-preview))


(when (locate-library "triton-ssh")
  (require 'triton-ssh))



;; See http://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))



(when (locate-library "irfc")
  (add-to-list 'auto-mode-alist '("[rR][fF][cC].*\\.txt" . irfc-mode)))



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

;;(global-set-key [f2] 'ff-find-other-file)
;;(global-set-key [f3] 'dired-jump)


;; On Emacs version 25, especiall on text terminal, the key `M-['
;; conflict with other control sequences (e.g. cannot use 'paste the
;; clipboard text' feature of terminal to Emacs.
;;
;; See https://emacs.stackexchange.com/questions/28851/how-to-turn-off-bracketed-paste-mode
(unless window-system
  (global-unset-key (kbd "M-[")))


(uinit/summarize)


;;
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;;

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold cinsk/gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook 'append)

;;; Local Variables:
;;; coding: utf-8
;;; End:
