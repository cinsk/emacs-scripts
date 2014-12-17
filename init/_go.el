;; -*-emacs-lisp-*-

;;;
;;; Go configuration
;;;


(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq indent-tabs-mode 1)))

(when (locate-library "subword")
  (add-hook 'go-mode-hook
            (labmda ()
                    (subword-mode 1))))
