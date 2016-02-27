;; -*-emacs-lisp-*-

;;;
;;; Configuration for cc-mode (i.e. C, C++, Objective-C, Java, Pike,
;;; AWK, CORBA IDL.
;;;

(require 'cc-mode)


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


;;; C & C++
(add-hook 'c-mode-hook
          #'(lambda ()
              (safe-visit-tags-table (concat (file-name-as-directory
                                              user-emacs-directory)
                                             "TAGS.sys") t)))

(add-hook 'c++-mode-hook
          #'(lambda ()
              (safe-visit-tags-table (concat (file-name-as-directory
                                              user-emacs-directory)
                                             "TAGS.sys") t)))

(add-hook 'c-mode-hook (function (lambda nil (abbrev-mode 1))))
(add-hook 'c++-mode-hook (function (lambda nil (abbrev-mode 1))))

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

;;; Java
(add-hook 'java-mode-hook (lambda ()
                            (subword-mode 1)
                            (c-set-offset 'statement-cont '++)))

(when (locate-library "autodisass-java-bytecode")
  (require 'autodisass-java-bytecode))
