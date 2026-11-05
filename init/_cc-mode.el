;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;
;;; Configuration for cc-mode (i.e. C, C++, Objective-C, Java, Pike,
;;; AWK, CORBA IDL.
;;;

;; (require 'cc-mode)



;;; TODO: https://github.com/torvalds/linux/blob/master/Documentation/process/coding-style.rst


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
(with-eval-after-load "cc-mode"
  (add-hook 'c-mode-hook
            #'(lambda ()
                (my/visit-tags-table (file-name-concat user-emacs-directory
                                                       "TAGS.sys") t)))

  (add-hook 'c++-mode-hook
            #'(lambda ()
                (my/visit-tags-table (file-name-concat user-emacs-directory
                                                       "TAGS.sys") t)))

  (add-hook 'c-mode-hook (function (lambda nil (abbrev-mode 1))))
  (add-hook 'c++-mode-hook (function (lambda nil (abbrev-mode 1))))

  (when (locate-library "cc-subword")
    (require 'cc-subword)
    (keymap-set c-mode-base-map "M-F" #'c-forward-subword)
    (keymap-set c-mode-base-map "M-B" #'c-backward-subword)
    (keymap-set c-mode-base-map "M-D" #'c-kill-subword))

  (keymap-set c-mode-base-map "M-{" #'c-beginning-of-defun)
  (keymap-set c-mode-base-map "M-}" #'c-end-of-defun)

  (keymap-set c-mode-base-map "C-M-{" #'c-up-conditional-with-else)
  (keymap-set c-mode-base-map "C-M-}" #'c-down-conditional-with-else)

  ;; Highlights suspicious C/C++ constructions
  (add-hook 'c-mode-common-hook (lambda () (cwarn-mode 1)))

  ;; Java
  (add-hook 'java-mode-hook (lambda ()
                              (subword-mode 1)
                              (c-set-offset 'statement-cont '++)))

  ;; Prompt for arguments to the preprocessor for `c-macro-expand'
  (setq c-macro-prompt-flag t)

  (when (locate-library "autodisass-java-bytecode")
    (require 'autodisass-java-bytecode)))
