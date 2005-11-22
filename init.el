;;; -*-emacs-lisp-*-

;;; $Id$

;;;
;;; Seong-Kook Shin's .emacs initialization file.
;;;

(setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))

;(server-start)

;;; Set up the keyboard so the delete key on both the regular keyboard
;;; and the keypad delete the character under the cursor and to the right
;;; under X, instead of the default, backspace behavior.
;;; (global-set-key [delete] 'delete-char)
;;; (global-set-key [kp-delete] 'delete-char)

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
(setq auto-mode-alist (cons '("/linux.*/.*\\.[ch]$" . linux-c-mode)
                            auto-mode-alist))

;; (setq-default make-backup-files nil)

;;;
;;; for emacs-20.4, korean
;;;

;;; set input method toggle key to 'Shift-Space'
(global-set-key [?\S- ] 'toggle-input-method)
(setq-default default-korean-keyboard "3")


;;; From Mr. Shin's FAQ
;;; and jay's setup <http://pllab.kaist.ac.kr/~jay>
;;; General korean langauge environment setting
(require 'cl)
(when enable-multibyte-characters
  (set-language-environment "Korean")
  (setq-default file-name-coding-system 'utf-8)
  ;; comment out if you use 3 bulsik
  (setq default-korean-keyboard "3")
  (setq default-input-method "korean-hangul3")
  (setq input-method-verbose-flag nil
        input-method-highlight-flag nil)
  ;;;; give highest priority to euc-kr
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)

  (add-hook 'quail-inactivate-hook 'delete-quail-completions)
  (defun delete-quail-completions ()
    (when (get-buffer "*Quail Completions*")
      (kill-buffer "*Quail Completions*")))
  ;(set-selection-coding-system 'euc-kr)
  (set-selection-coding-system 'utf-8)

  ;;(unless window-system
  ;;(menu-bar-mode -1)
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

(defun unicode-shell ()
  "Execute the shell buffer in UTF-8 encoding.
Note that you'll need to set the environment variable LANG and others 
appropriately."
  (interactive)
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (coding-system-require-warning t))
    (call-interactively 'shell)))


;;;
;;; Buffer Menu
;;;
;;; Sort by the 2nd column (buffer name) in Buffer list
(setq Buffer-menu-sort-column 2)

;;; for wheel mouse
;;;
;;;  http://www.inria.fr/koala/colas/mouse-wheel-scroll/#gnuemacs
;;;
;;(defun up-slightly () (interactive) (scroll-up 5))
;;(defun down-slightly () (interactive) (scroll-down 5))
;;(global-set-key [mouse-4] 'down-slightly)
;;(global-set-key [mouse-5] 'up-slightly)
;;(defun up-one () (interactive) (scroll-up 1))
;;(defun down-one () (interactive) (scroll-down 1))
;;(global-set-key [S-mouse-4] 'down-one)
;;(global-set-key [S-mouse-5] 'up-one)
(cond (window-system
       (mwheel-install)
       ))

;;;
;;; Force mouse yanks at point not at cursor.
;;;
(setq mouse-yank-at-point t)

;; frame title : set to buffer name
;;(setq frame-title-format "Emacs - %f ")  
(setq frame-title-format (if window-system
			     "%F - %f"
			   "Emacs - %f"))
(setq icon-title-format  "%b")

;;(defun up-a-lot () (interactive) (scroll-up))
;;(defun down-a-lot () (interactive) (scroll-down))
;;(global-set-key [C-mouse-4] 'down-a-lot)
;;(global-set-key [C-mouse-5] 'up-a-lot)


;;;
;;; If you want to scroll by half a page instead of only 5 lines as above,
;;; John Rowe sent this GNU Emacs code: 
;;;
;;(defun scroll-up-half ()
;;  "Scroll up half a page."
;;  (interactive)
;;  (scroll-up (/ (window-height) 2)))
;;
;;(defun scroll-down-half ()
;;  "Scroll down half a page."
;;  (interactive)
;;  (scroll-down (/ (window-height) 2)))
;;
;;(global-set-key [(mouse-5)] 'scroll-up-half)
;;(global-set-key [(mouse-4)] 'scroll-down-half)


;;;
;;; If you are intended BS (backspace) key to work
;;; correctly on some terminals, uncomment one of below s-exp.
;;;                                                 -- cinsk
;;(global-set-key [C-?] 'backward-delete-char)
;;(global-set-key [C-h] 'backward-delete-char);

;;;
;;; Emacs Lisp escape sequence in a string:
;;; <TAB> - `\t'  <RET> - `\r'  <ESC> - `\e'  <DEL> - `\d'
;;;
;;; To use function keys, mouse button, or non-ASCII character such
;;; as `C-=' or `H-a', use a vector(`[..]') to specify the key sequence.
;;;
;;;  - If a vector element is a character, use the Lisp character constant,
;;;    `?'. e.g. `?\C-='
;;;  - For example, below two statements are the same:
;;;     (global-set-key "\C-x\e\e" 'repeat-complex-command)
;;;     (global-set-key [?\C-x ?\e ?\e] 'repeat-complex-command)
;;;
;;; Lisp Symbols for the function keys:
;;;  `left', `up', `right', `down'
;;;       Cursor arrow keys.
;;;  
;;;  `begin', `end', `home', `next', `prior'
;;;       Other cursor repositioning keys.
;;;  
;;;  `select', `print', `execute', `backtab'
;;;  `insert', `undo', `redo', `clearline'
;;;  `insertline', `deleteline', `insertchar', `deletechar'
;;;       Miscellaneous function keys.
;;;  
;;;  `f1', `f2', ... `f35'
;;;       Numbered function keys (across the top of the keyboard).
;;;  
;;;  `kp-add', `kp-subtract', `kp-multiply', `kp-divide'
;;;  `kp-backtab', `kp-space', `kp-tab', `kp-enter'
;;;  `kp-separator', `kp-decimal', `kp-equal'
;;;       Keypad keys (to the right of the regular keyboard), with names or
;;;       punctuation.
;;;  
;;;  `kp-0', `kp-1', ... `kp-9'
;;;       Keypad keys with digits.
;;;  
;;;  `kp-f1', `kp-f2', `kp-f3', `kp-f4'
;;;       Keypad PF keys.
;;;

(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cd" 'unicode-shell)

(global-set-key [?\C-.] 'find-tag-other-window) ; C-x o 
(global-set-key "\C-c\C-l" 'goto-line)
(global-set-key "\C-c\C-i" 'indent-region)

;;; C-x C-v is binded find-alternate-file by default.
(global-set-key "\C-x\C-v" 'view-file)


;;;(defun my-c-mode-hook ()
;;;  (local-set-key "\C-cc" 'compile)
;;;  (local-set-key "\C-cs" 'shell))
;;;
;;;(add-hook 'c-mode-hook 'my-c-mode-hook)

(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   (visit-tags-table "/usr/share/emacs/TAGS")))

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
(add-hook 'c++-mode-hook (function (lambda nil (abbrev-mode 1))))

;;; imenu mode
;;;(add-hook 'c-mode-hook (function (lambda nil (imenu-add-to-menubar))))
;;;(add-hook 'c++-mode-hook (function (lambda nil (imenu-add-to-menubar))))

;;; which-function mode
;;;(add-hook 'c-mode-hook (function (lambda nil (which-function-mode))))
;;;(add-hook 'c++-mode-hook (function (lambda nil (which-function-mode))))

(which-function-mode 1)			; display function names in mode-line
(iswitchb-mode 1)			; smart buffer switching mode

(global-font-lock-mode 1)		; every buffer uses font-lock-mode
(line-number-mode 1)			; show line number in mode-line
(column-number-mode 1)			; show column number in mode-line

(setq resize-minibuffer-mode t)		; ensure all contents of mini
					; buffer visible

(tool-bar-mode -1)			; hide tool bar

;;(when window-system
;;  (setq special-display-buffer-names
;;        '("*Completions*" "*grep*" "*Buffer List*")))

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

(add-hook 'c-mode-hook '(lambda () 
                          (make-local-variable 'write-contents-hooks)
                          (add-hook 'write-contents-hooks 'source-untabify)))
(add-hook 'c++-mode-hook '(lambda () 
                            (make-local-variable 'write-contents-hooks)
                            (add-hook 'write-contents-hooks 'source-untabify)))

(when nil
  (load-library "gtags")
  (add-hook 'c-mode-hook '(lambda () (gtags-mode 1)))
  (add-hook 'c++-mode-hook '(lambda () (gtags-mode 1))))

;;;
;;; Colors
;;;
;;;(set-background-color "rgb:0000/1500/8000")
;;;(set-foreground-color "white")
;;;(set-cursor-color "")
;;;(set-mouse-color "")
;;;(set-face-foreground 'highlight "white")
;;;(set-face-background 'highlight "slate blue")
;;;(set-face-background 'region "slate blue")
;;;(set-face-background 'secondary-selection "turquoise")

;;;
;;; emacs server
;;;
;;;(load "/usr/share/emacs/21.2/lisp/gnuserv")
(server-start)
;;;(load "gnuserv")
;;;(gnuserv-start)

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


;;;
;;; navigation customization
;;;

(require 'cc-mode)
(add-hook 'c-mode-common-hook (lambda () (cwarn-mode 1)))

;(define-key c-mode-base-map [(meta ?F)] 'c-forward-into-nomemclature)
;(define-key c-mode-base-map [(meta ?B)] 'c-backward-into-nomemclature)
(define-key c-mode-base-map [(meta ?{)] 'c-beginning-of-defun)
(define-key c-mode-base-map [(meta ?})] 'c-end-of-defun)
(define-key c-mode-base-map [(control meta ?{)] 'c-up-conditional-with-else)
(define-key c-mode-base-map [(control meta ?})] 'c-down-conditional-with-else)

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
(global-set-key [(control tab)] 'other-window)
(global-set-key [(control x) ?w ?n] 'other-window)
(global-set-key [(control x) ?w ?o] 'other-window)
(global-set-key [(control x) ?w ?p] 'reverse-other-window)
(global-set-key [(control x) ?w ?k] 'delete-window)
(global-set-key [(control x) ?w ?K] 'delete-other-window)

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


;(require 'autofit-frame)
;(add-hook 'after-make-frame-functions 'fit-frame)
;
;(add-hook 'temp-buffer-show-hook
;          'fit-frame-if-one-window 'append)

;;;
;;; psgml mode setup
;;;
;(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
;(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

(require 'nxml-mode)
(setq auto-mode-alist (cons '("\\.xml\\|.pvm" . nxml-mode)
      auto-mode-alist))

;;;
;;; Dired and dired-x setting
;;;
(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")
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
	    ;; (dired-omit-mode 1)
	    ))

(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files
	      ;; Omit RCS files
	      "\\|^RCS$\\|,v$"
	      ;; Omit CVS and Bitkeeper files
	      "\\|^CVS$\\|^BitKeeper$"
	      ;; Omit dot files
	      "\\|^\\..+$"
	      ;; Omit .o, .lo, .Po, .Plo, .a, .la files
	      "\\|.+\\.\\(o\\|lo\\|Po\\|Plo\\|a\\|la\\)$"))


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
(require 'xcscope)

;;;
;;; Version Control
;;;
(global-set-key [(control x) (control q)] 'vc-toggle-read-only)


;;(when window-system
(when nil
  (setq same-window-buffer-names 
        (append '(;"*compilation*"
                  "*Process List*")
                  same-window-buffer-names))

  (setq special-display-regexps 
        '(("\\*Buffer List\\*"
           (font . "fixed")
           (left . 0)                   ; in pixels
           (top . -30)
           (auto-raise . t)
           (width . 70)
           (height . 10)                ; in characters
           (vertical-scroll-bars . nil)
           (tool-bar-lines . nil)
           (menu-bar-lines . nil))
          (("\\*Completions\\*"
           (font . "fixed")
           (left . 0)                   ; in pixels
           (top . -30)
           (auto-raise . f)
           (width . 70)
           (height . 10)                ; in characters
           (vertical-scroll-bars . nil)
           (tool-bar-lines . nil)
           (menu-bar-lines . nil))
          ("\\*cscope\\*"
           (font . "fixed")
           (left . 0)
           (top . 0)
           (auto-raise . t)
           (width . 80)
           (height . 20)
           (vertical-scroll-bars . nil)
           (tool-bar-lines . nil)
           (menu-bar-lines . nil))
          ("\\*.*\\*"
           (tool-bar-lines . nil)
           (menu-bar-lines . nil))
))))



;;(load "~/.emacs.d/theme")

;;(split-window-horizontally)

(setq explicit-bash-args (quote ("--noediting" "-i" "-l")))

(setq gnus-select-method '(nntp "news.kornet.net"))


(defun select-random-color-theme ()
  "Select random color theme"
  (interactive)
  (random t)
  (let* ((index (+ (random (- (length color-themes) 2)) 2))
         (theme (nth index color-themes))
         (name (symbol-name (car theme))))
    (message "%s installed" name)
    (funcall (car theme))))

(defun set-frame-color-theme (frame)
  (select-frame frame)
  (select-random-color-theme))

(when window-system
  (require 'color-theme)
  (require 'pink-bliss)
  (require 'cinsk-wood)
  (global-set-key [(control f1)] 'select-random-color-theme)
  (add-hook 'after-make-frame-functions 'set-frame-color-theme)
  
  ;(color-theme-deep-blue)
  (setq color-theme-is-global nil)
  (color-theme-robin-hood)
  
  (set-face-font 'default "fontset-lucida14")
  )

;;;
;;; Emacs-wiki support
;;;
(require 'emacs-wiki)

;;;
;;; Display splash screen on startup
;;;
(fancy-splash-screens)
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


;(put 'narrow-to-region 'disabled nil)
