;; -*-emacs-lisp-*-

;;; From Mr. Shin's FAQ
;;; and jay's setup <http://pllab.kaist.ac.kr/~jay>
;;; General korean langauge environment setting
(require 'cl)

;;; set input method toggle key to 'Shift-Space'
(global-set-key [?\S- ] 'toggle-input-method)

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

