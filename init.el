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


(defvar cinsk/loaded-init-files nil
  "List of loaded snippets.")

(defvar cinsk/snippets-directory
  (concat (expand-file-name user-emacs-directory)
          "snippets")
  "Directory contains the init snippets.")

(defmacro cinsk/load-snippet (snippet &rest body)
  "If the last sexp of the BODY results non-nil, load the init script, SNIPPET.

\(fn SNIPPET BODY...)"
  (declare (indent 1) (debug t))
  (let ((sname (make-symbol "SNIPPET")))
    `(let* ((,sname ,snippet)
            (absname (if (file-name-absolute-p ,sname)
                         ,sname
                       (concat (expand-file-name ,cinsk/snippets-directory)
                               "/" ,sname))))
       (unless (member absname cinsk/loaded-init-files)
         (let ((pred (progn ,@body)))
           (when pred
             (condition-case err
                 (progn
                   (load absname)
                   (message "%s loaded" (file-name-nondirectory ,sname))
                   (add-to-list 'cinsk/loaded-init-files absname)
                   t)
               (error (lwarn 'dot-emacs :warning
                             (format "%s: %s: %S" ,sname
                                     (car err) (cdr err)))))))))))


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


(cinsk/load-snippet "darwin"
  (eq system-type 'darwin))


(cinsk/load-snippet "X"
  (eq window-system 'x))


(cinsk/load-snippet "ediff"
  ;; Note that some external packages loads 'ediff by themselves, such
  ;; as magit and color-theme.  Since
  ;; `ediff-make-wide-display-function' should be set before loading
  ;; `ediff, ediff customization should be placed in the first
  ;; place. -- cinsk
  'ediff)


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

(cinsk/load-snippet "fonts"
  'fonts)


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



(cinsk/load-snippet "cc-mode"
                    'cc-mode)



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



(cinsk/load-snippet "shell"
                    'shell)


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

;; navigation


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


(cinsk/load-snippet "delete"
                    'delete)


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



(cinsk/load-snippet "window-frame"
                    'window-frame)

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


(cinsk/load-snippet "latex"
                    'latex-mode)


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



(cinsk/load-snippet "mmm-mode"
  (let ((mmm-dir (expand-file-name 
                  (concat (file-name-as-directory user-emacs-directory)
                          "mmm-mode"))))
    ;; If MMM mode is installed in $HOME/.emacs.d/mmm-mode/
    (when (file-accessible-directory-p mmm-dir)
      (add-to-list 'load-path mmm-dir)
      (add-to-list 'Info-directory-list mmm-dir))
    (locate-library "mmm-auto")))



(cinsk/load-snippet "nxml"
  (or (locate-library "nxml-mode")
      ;; For legacy nxml-mode which does not use `provide' for nxml-mode.
      (locate-library "rng-auto")))


(cinsk/load-snippet "dired"
  'dired)


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


(cinsk/load-snippet "color" 'color-theme)



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


(cinsk/load-snippet "org" (locate-library "org"))


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



(cinsk/load-snippet "ruby"
  (locate-library "ruby-mode"))

(cinsk/load-snippet "python"
  (locate-library "python-mode"))


(cinsk/load-snippet "maven" 'maven)


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


(cinsk/load-snippet "scala"
  ;; Currently, Scala 2.8.x is not provided by gentoo portage. Thus, I
  ;; will use the binary distribution from the Scala repository in
  ;; /opt/scala
  (when (not (locate-library "scala-mode-auto"))
    (let* ((scala-mode-path "/opt/scala/misc/scala-tool-support/emacs")
           (scala-file (concat (file-name-as-directory scala-mode-path)
                               "scala-mode-auto.el")))
      (if (file-exists-p scala-file)
          (add-to-list 'load-path scala-mode-path))))
  (locate-library "scala-mode-auto"))



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
