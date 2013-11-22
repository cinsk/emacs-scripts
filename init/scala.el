;; -*-emacs-lisp-*-


;; Currently, Scala 2.8.x is not provided by gentoo portage. Thus, I
;; will use the binary distribution from the Scala repository in
;; /opt/scala
(when (not (locate-library "scala-mode-auto"))
  (let* ((scala-mode-path "/opt/scala/misc/scala-tool-support/emacs")
         (scala-file (concat (file-name-as-directory scala-mode-path)
                             "scala-mode-auto.el")))
    (if (file-exists-p scala-file)
        (add-to-list 'load-path scala-mode-path))))


(eval-after-load "scala-mode"
  '(progn
     ;; Modify scala-mode-map to keep consistency with other
     ;; interpreter setting.
     ;;
     ;; C-c C-b   scala-eval-buffer
     ;; C-c C-r   scala-eval-region
     ;; C-c C-e   scala-eval-definition  (TODO: check the symantics)
     (define-key scala-mode-map [(control ?c) (control ?e)]
       'scala-eval-definition)
     ;;
     ;; scala-undent-line: `C-<tab>' -> `<backtab>'
     (move-key scala-mode-map
               [(control tab)] [backtab])

     (define-key scala-mode-map [(control ?c) ?\!] 'scala-run-scala)
     ))

(when (locate-library "scala-mode-auto")
  (require 'scala-mode-auto))
