;;; -*-emacs-lisp-*-

;;; $Id$

;;;
;;; Seong-Kook Shin's .emacs initialization file.
;;;

;;;
;;; emacs packages for my personal uses are placed in $HOME/.emacs.d
;;;
(setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))


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


(when (xftp)
  ;; When Emacs uses Xft font backend, "control + mouse wheel up"
  ;; increases the default font size whereas "control + mouse wheel
  ;; down " decreases the size.

  ;; Note that if you call `mwheel-install' after this configuration,
  ;; both [C-mouse-4] and [C-mouse-5] bindings are cleared.
  ;;
  ;; It seems that mwheel package is automatically loaded in Emacs 22
  ;; or higher.  Thus, I do not need to call `mwheel-install' any longer.
  (global-set-key [C-mouse-4] (lambda ()
                                (interactive)
                                (scale-default-font-height 1.1
                                                           (selected-frame))
                                (message "New face height: %d" 
                                         (face-attribute 'default :height))))
  (global-set-key [C-mouse-5] (lambda ()
                                (interactive)
                                (scale-default-font-height 0.9
                                                           (selected-frame))
                                (message "New face height: %d" 
                                         (face-attribute 'default :height)))))

;;; Although it is possible to set font faces in lisp code, I prefer
;;; to use X resource configuration.
;;;
(when nil
  ; "NanumGothic_Coding-12"
  ;(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))
  ;(set-fontset-font "fontset-default" 'hangul
  ;'("NanumGothic_Coding-12" . "unicode-bmp"))
  (set-face-font 'default "fontset-default")
  (set-fontset-font "fontset-default" '(#x1100. #xffdc)
                    '("NanumGothic_Coding" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'ascii
                    '("NanumGothic_Coding" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'latin-iso8859-1
                    '("NanumGothic_Coding" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'hangul
                    '("NanumGothic_Coding" . "unicode-bmp"))
  (set-fontset-font "fontset-default" '(#xe0bc. #xf66e)
                    '("NanumGothic_Coding" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'kana
                    '("NanumGothic_Coding" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'han
                    '("NanumGothic_Coding" . "unicode-bmp"))
)

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
  ; (setq-default file-name-coding-system 'utf-8)

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
  ;(set-selection-coding-system 'euc-kr)
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
;;; Shell configuration
;;;

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

(global-set-key "\C-cd" 'unicode-shell)

;; Allow shell mode to handle color output from shell commands
;; (notably from ls --color command)
;;
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;
;; Make the inferior shell a login shell.
;;
(setq explicit-bash-args (quote ("--noediting" "-i" "-l")))


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
         ("emacs" (or
                   (name . "^\\*scratch\\*$")
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
(setq frame-title-format "%F - %b")
(setq icon-title-format  "%b")


;;;
;;; If you are intended BS (backspace) key to work
;;; correctly on some terminals, uncomment one of below s-exp.
;;;                                                 -- cinsk
;;(global-set-key [C-?] 'backward-delete-char)
;;(global-set-key [C-h] 'backward-delete-char)


(global-set-key "\C-cc" 'compile)


(global-set-key [?\C-.] 'find-tag-other-window) ; C-x o 


(global-set-key [(control c) ?i] 'indent-region)

;;;
;;; ICE setup
;;;
(add-to-list 'auto-mode-alist '(".*\\.ice$" . java-mode))

;;;
;;; cc-mode
;;;

(require 'cc-mode)

(add-hook 'c-mode-hook
          #'(lambda ()
              (safe-visit-tags-table "~/.emacs.d/TAGS.sys" t)))
             
(add-hook 'c++-mode-hook
          #'(lambda ()
              (safe-visit-tags-table "~/.emacs.d/TAGS.sys" t)))

(define-abbrev-table 'c-mode-abbrev-table 
  ;; I don't know why `@' for abbreviation doesn't work.
  ;; So I choose `$' for that.
  '(("$niy" "/* TODO: Not Implemented Yet. */" nil 0)

    ("$gpl" 
"/*
 * <one line to give the program's name and a brief idea of what it does.>
 * Copyright (C) <year>  <name of author>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */" nil 0)
    ("$lgpl"
"/* <one line to give the library's name and a brief idea of what it does.>
 * Copyright (C) <year>  <name of author>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */" nil 0)
    ("$igpl"
"static const char *gpl_notices[] = {
  \"PROGRAM-NAME version XXX, Copyright (C) YEAR AUTHOR-NAME\",
  \"PROGRAM-NAME comes with ABSOLUTELY NO WARRANTY; for details type `show w'.\",
  \"This is free software, and you are welcome to redistribute it\",
  \"under certain conditions; type `show c' for details.\",
};" nil 0)))
(add-hook 'c-mode-hook (function (lambda nil (abbrev-mode 1))))


(define-abbrev-table 'c++-mode-abbrev-table 
  ;; I don't know why `@' for abbreviation doesn't work.
  ;; So I choose `$' for that.
  '(("$niy" "// TODO: Not Implemented Yet." nil 0)
    ("@niy" "// TODO: Not Implemented Yet." nil 0)
    ("$cxxchk" "#ifndef __cplusplus
#error This is a C++ header file
#endif" nil 0)
    ("$gpl" 
"/*
 * <one line to give the program's name and a brief idea of what it does.>
 * Copyright (C) <year>  <name of author>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 
 * 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */" nil 0)
    ("$lgpl"
"/* <one line to give the library's name and a brief idea of what it does.>
 * Copyright (C) <year>  <name of author>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */" nil 0)
    ("$igpl"
"static const char *gpl_notices[] = {
  \"PROGRAM-NAME version XXX, Copyright (C) YEAR AUTHOR-NAME\",
  \"PROGRAM-NAME comes with ABSOLUTELY NO WARRANTY; for details type `show w'.\",
  \"This is free software, and you are welcome to redistribute it\",
  \"under certain conditions; type `show c' for details.\",
};" nil 0)))
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



;;;
;;; Switching between buffers using iswitchb
;;;
(iswitchb-mode 1)			; smart buffer switching mode
(setq iswitchb-default-method 'maybe-frame) ; ask to use another frame.


;;;

;;;
;;; Minor Mode configuration
;;;

;; imenu mode
;;(add-hook 'c-mode-hook (function (lambda nil (imenu-add-to-menubar))))
;;(add-hook 'c++-mode-hook (function (lambda nil (imenu-add-to-menubar))))

;;(add-hook 'c-mode-hook (function (lambda nil (which-function-mode))))
;;(add-hook 'c++-mode-hook (function (lambda nil (which-function-mode))))

(which-function-mode 1)          ; display function names in mode-line

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


(defun zap-to-nonspace ()
  "Delete all whitespace up to the next non-whitespace char."
  (interactive)
  (save-excursion
    (let ((start (point))
          (end (point-max)))
      (if (re-search-forward "[^ \n\t\v]" nil t)
          (setq end (min (1- (point)) end)))
      (kill-region start end))))

;;
;; C-c C-d deletes all whitespaces up to the next non-whitespace character.
;;
(global-set-key [(control ?c) (control ?d)] 'zap-to-nonspace)



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

;(global-set-key [up]   '(lambda () (interactive) (scroll-down 1)))
;(global-set-key [down] '(lambda () (interactive) (scroll-up 1)))

(fset 'scroll-other-frame "\C-xo\C-v\C-xo")      ; C-x o C-v C-x o
(fset 'scroll-other-frame-down "\C-xo\366\C-xo") ; C-x o M-v C-x o

(global-set-key [(meta shift prior)] 'scroll-other-frame-down)
(global-set-key [(meta shift next)] 'scroll-other-frame)



(global-set-key [(control meta ?\])] #'forward-page)
(global-set-key [(control meta ?\[)] #'backward-page)


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

;(global-set-key [C-tab] 'other-window)  ; C-x o
;(global-set-key [S-iso-lefttab] 'reverse-other-window)
;(global-set-key [(backtab)] 'reverse-other-window)
(global-set-key [(control tab)] 'smart-other-window)
(global-set-key [(control x) ?w ?n] 'other-window)
(global-set-key [(control x) ?w ?o] 'other-window)
(global-set-key [(control x) ?w ?p] 'reverse-other-window)
(global-set-key [(control x) ?w ?k] 'delete-window)
(global-set-key [(control x) ?w ?K] 'delete-other-window)

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


(defun line-numbers-on-region (&optional start)
  "Insert line numbers on the current region.
A numeric prefix argument specifies the starting number"
  (interactive "P")
  (let ((start (if (null start) 1 (prefix-numeric-value start)))
        (begin (region-beginning))
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


;(require 'autofit-frame)
;(add-hook 'after-make-frame-functions 'fit-frame)
;
;(add-hook 'temp-buffer-show-hook
;          'fit-frame-if-one-window 'append)


;;;
;;; Emacs Lisp Mode
;;;

(add-hook 'emacs-lisp-mode-hook 
          '(lambda ()
             (safe-visit-tags-table "~/.emacs.d/TAGS.emacs" t)))

(eval-after-load "lisp-mode"
  '(progn
    (define-key emacs-lisp-mode-map [f5] 'eval-buffer)
    (define-key emacs-lisp-mode-map [(control c) ?|] 'eval-region)))


;;;
;;; Common Lisp Mode -- from clisp-2.38/editors.txt
;;;
;;; It seems that Emacs already have `lisp-eval-last-sexp' that has
;;; the same feature of `

;; clisp does not work with slime package for now -- cinsk
;;(setq inferior-lisp-program "clisp -I -q -E utf-8")
(setq inferior-lisp-program "sbcl --noinform")

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
(when (locate-library "slime")
  (eval-after-load "slime" '(slime-setup))
  (autoload 'slime "slime"
    "Start an inferior lisp and connect to its Swank server" t))



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
;;; XML configuration
;;;
(defun lzx-nxml-mode ()
  "OpenLaszlo XML Mode"
  (interactive)
  (nxml-mode)
  (make-local-variable 'nxml-child-indent)
  (setq nxml-child-indent 4))

(when (locate-library "rng-auto")
  (load (locate-library "rng-auto"))
  ;; `sgml-mode' adds an entry to `magic-mode-alist' so that
  ;; `auto-mode-alist' to `nxml-mode' might not work.  To work around
  ;; this, define an alias for `xml-mode' to `nxml-mode'.
  (defalias 'xml-mode 'nxml-mode)
  ;(autoload 'nxml-mode "nxml-mode" "new XML major mode" t)
  (setq auto-mode-alist (cons '("\\.\\(xml\\|pvm\\|rss\\)\\'" . nxml-mode)
                              auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.lzx\\'" . lzx-nxml-mode)
                              auto-mode-alist)))

(define-abbrev-table 'nxml-mode-abbrev-table 
  ;; I don't know why `@' for abbreviation doesn't work.
  ;; So I choose `$' for that.
  '(("$doctypexhtml" 
"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
	\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" nil 0)
))
(add-hook 'nxml-mode-hook (function (lambda nil (abbrev-mode 1))))

;; Make a slash automatically completes the end-tag
(eval-after-load "nxml-mode"
  '(progn
     (setq nxml-slash-auto-complete-flag t)
     (define-key nxml-mode-map [(control ?c) (control ?e)]
       'nxml-enclose-paragraph)))


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
          ;(message (format "curpos(%d) point(%d)" curpos (point)))
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
                ;(message (format "pt(%d) pt-max(%d)" (point) (point-max)))
                (if (>= (point) (1- (point-max)))
                    (setq done t))
                )))))))


;;;
;;; Dired and dired-x setting
;;;
(require 'dired-x)

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

(setq-if-equal dired-garbage-files-regexp
               "\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\)\\'"
               (format "\\(?:%s\\|%s\\)\\'"
                       "aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc" ; TeX related
                       "\\`\.#.*[0-9]"))                          ; VC related

;(define-key global-map "\C-x\C-j" 'dired-jump)
;(define-key global-map "\C-x4\C-j" 'dired-jump-other-window)
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
  (let ((file (buffer-file-name)))
    (and (not (eq (file-uid file) (user-uid)))
         (view-mode 1))))

;(add-hook 'find-file-hook 'smart-view-mode)

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
;;; News Reader
;;;
;(setq gnus-select-method '(nntp "news.kornet.net"))
(setq gnus-select-method '(nntp "public.teranews.com"))



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

(defvar color-theme-favorites '(color-theme-deep-blue
                                color-theme-cinsk-wood
                                color-theme-charcoal-black
                                color-theme-clarity
                                color-theme-comidia
                                color-theme-dark-blue2
                                color-theme-dark-laptop
                                color-theme-taylor
                                color-theme-robin-hood)
      "My favorite color theme list")

(defun color-theme-select-random ()
  "Select random color theme"
  (interactive)
  (let ((sym (car (nth (random (length color-themes)) color-themes))))
    (funcall sym)
    (message "%s installed" (symbol-name sym)))
  (if nil
      ;; Below code was my first creation.  Don't know which is better yet.
      (progn
        (random t)
        (let* ((index (+ (random (- (length color-themes) 2)) 2))
               (theme (nth index color-themes)))
          (save-font-excursion 'default
            (funcall (car theme)))
          (message "%s installed" (symbol-name (car theme)))))))


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
        ((eq arg :random)  (color-theme-select-random))
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


(defun set-frame-color-theme (frame)
  (select-frame frame)
  (color-theme-select-random))

(when (and window-system
           (locate-library "color-theme"))
  (require 'color-theme)
  (color-theme-initialize)

  (and (locate-library "pink-bliss")
       (require 'pink-bliss))

  (and (locate-library "cinsk-wood")
       (require 'cinsk-wood))

  (global-set-key [(control f1)] 'color-theme-select-random)
  (global-set-key [(control f2)] '(lambda ()
                                    (interactive)
                                    (color-theme-apply :next)))
  (add-hook 'after-make-frame-functions 'set-frame-color-theme)

  ;; color-theme-* is frame-local from now.
  (setq color-theme-is-global nil)

  ;; Select random color theme from my favorite list
  (let ((theme (nth (random (length color-theme-favorites))
                    color-theme-favorites))
        (buf "*scratch*"))
    (funcall theme))
  )


;;;
;;; CSS mode
;;;
(autoload 'css-mode "css-mode" "CSS editing major mode" t)
(eval-after-load "css-mode"
  '(setq cssm-indent-function #'cssm-c-style-indenter))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))


;;;
;;; Calender
;;;
(setq calendar-date-display-form
      '(year "-" month "-" day (if dayname (concat ", " dayname))))
(setq mark-holidays-in-calendar t)
(setq mark-diary-entries-in-calendar t)
(add-hook 'diary-display-hook 'fancy-diary-display)

(setq general-holidays
      '((holiday-fixed 1 1 "설날")
        (holiday-fixed 3 1 "삼일절")
        (holiday-fixed 4 5 "식목일")
        (holiday-fixed 5 5 "어린이날")
        ;;(holiday-fixed 5 8 "어버이날")
        ;;(holiday-fixed 5 15 "스승의날")
        (holiday-fixed 6 6 "현충일")
        (holiday-fixed 7 17 "제헌절")
        (holiday-fixed 8 15 "광복절")
        ;;(holiday-fixed 10 1 "국군의 날")
        (holiday-fixed 10 3 "개천절")
        ;;(holiday-fixed 10 9 "한글날")
        (holiday-fixed 12 25 "성탄절")))

(setq local-holidays
      '((holiday-fixed 11 1 "삼성전자 창립일")))


;;;
;;; Org mode
;;;

;; org-hide-leading-stars should be set before loading org-mode.
(setq org-hide-leading-stars t)
(setq org-odd-levels-only t)
(setq org-agenda-include-diary t)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key [(control c) ?a] 'org-agenda)
(global-set-key [(control c) ?l] 'org-store-link)

(defvar org-table-convert-last-nrows	3
  "Default number of columns per row.  This is changed if user used
another value")

(defun org-table-convert-from-lines (&optional nrows)
  "Convert lines to the org table. Each line contains one column so that
users need to specify the number of columns per row.

For example, if the region contains 9 lines and each line contains the digit from
1 to 9, calling `org-table-convert-from-lines' with the column number 3 makes the
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
     (move-key org-mode-map [(control c) (control d)] [(control c) ?e])

     (define-key org-mode-map [(control c) (control ?\\)]
       'org-table-convert-from-lines)

     ;; When opening a link with `org-open-at-point' (C-c C-o), These
     ;; settings allow to use acroread for pdf files and to use ggv
     ;; for ps files.
     (add-to-list 'org-file-apps '("pdf" . "acroread %s"))
     (add-to-list 'org-file-apps '("ps" . "ggv %s"))))


;;;
;;; Emacs-wiki support
;;;
;(require 'emacs-wiki)


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
;;; Ediff customization
;;;
(eval-after-load "ediff"
  '(progn
     ;; ignore whitespaces and newlines
     (setq ediff-ignore-similar-regions t)
     ;; do not create new frame for the control panel
     (setq ediff-window-setup-function 'ediff-setup-windows-plain)
     ))


;;;
;;; Do not display splash screen on startup
;;;

;; Show the `*scratch*' buffer 
(setq initial-buffer-choice t)

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
;;; python-mode
;;;

(when (locate-library "python-mode")
  (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
  (setq interpreter-mode-alist (cons '("python" . python-mode)
                                     interpreter-mode-alist))
  (autoload 'python-mode "python-mode" "Python editing mode." t))


(eval-after-load "python-mode"
  '(progn
     ;; python-mode uses `C-c C-c' for `py-execute-buffer' where most
     ;; major modes uses that for `comment-region'.  Thus, I'll uses
     ;; `C-c C-e' bindings for py-execute-buffer.  It makes sense
     ;; because cc-mode uses this for `c-macro-expand'.
     (define-key py-mode-map [(control ?c) (control ?c)] 'comment-region)
     (define-key py-mode-map [(control ?c) (control ?e)] 'py-execute-buffer)

     ;; python-mode uses `C-c C-d' for `py-pdbtrack-toggle-stack-tracking'
     (define-key py-mode-map [(control ?c) (control ?d)] 'zap-to-nonspace)))

;;;
;;; w3m
;;;
(when (locate-library "w3m")
  (require 'w3m-load))



;;;
;;; gnuplot
;;;
(when (locate-library "gnuplot")
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)

  (setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode))
                                auto-mode-alist)))


;;;
;;; lua
;;;
(when (locate-library "lua-mode")
   (autoload 'lua-mode "lua-mode" "Major mode for lua script")
   (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))


;;;
;;; ESS(Emacs Speaks Statistics) setting for R.
;;;
(when (locate-library "ess-site")
  (require 'ess-site))


;;; To save & load Emacs session, following lines should be the last line
;;; in this file. 
;;;
;;; The first time you save the state of the Emacs session, you must do it
;;; manually, with the command `M-x desktop-save'. Once you have done that,
;;; exiting Emacs will save the state again--not only the present Emacs
;;; session, but also subsequent sessions. You can also save the state at
;;; any time, without exiting Emacs, by typing `M-x desktop-save' again.
;;;
;;; In order for Emacs to recover the state from a previous session, you
;;; must start it with the same current directory as you used when you
;;; started the previous session.  This is because `desktop-read' looks in
;;; the current directory for the file to read.  This means that you can have
;;; separate saved sessions in different directories; the directory in which
;;; you start Emacs will control which saved session to use.

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
(global-set-key [f12] 'calc)
(global-set-key [(control f12)] 'quick-calc)



;;(global-set-key [f2] 'ff-find-other-file)
;;(global-set-key [f3] 'dired-jump)
(global-set-key [f2] #'pop-to-cvs-buffer)

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
