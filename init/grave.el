;; As suggested in
;;  http://stackoverflow.com/questions/935723/find-tab-characters-in-emacs,
;;
;; Following sexp visualize tab character.
(when nil
  (add-hook 'font-lock-mode-hook
            '(lambda ()
               (when (memq major-mode
                           untabify-remove-trailing-spaces-on-write-modes)
                 (font-lock-add-keywords nil
                                         '(("\t" 0
                                            'trailing-whitespace prepend)))))))


(when nil
  (add-hook 'c-mode-hook '(lambda ()
                            (make-local-variable 'write-contents-hooks)
                            (add-hook 'write-contents-hooks 'cinsk/source-untabify)))
  (add-hook 'c++-mode-hook '(lambda ()
                              (make-local-variable 'write-contents-hooks)
                              (add-hook 'write-contents-hooks 'cinsk/source-untabify))))


(when nil
  ;; Support for GNU global, the source code tag system
  (load-library "gtags")
  (add-hook 'c-mode-hook '(lambda () (gtags-mode 1)))
  (add-hook 'c++-mode-hook '(lambda () (gtags-mode 1))))

(when nil
  (require 'autoinsert)

  (let ((aid_correct (path-join user-emacs-directory "insert"))
        (aid_default (if (boundp 'auto-insert-directory)
                         auto-insert-directory
                       "~/insert")))
    (setq auto-insert-directory
          (if (file-accessible-directory-p aid_correct)
              aid_correct
            aid_default)))

  (add-hook 'find-file-hook 'auto-insert))
