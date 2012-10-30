;; -*-emacs-lisp-*-

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

(require 'scala-mode-auto)
