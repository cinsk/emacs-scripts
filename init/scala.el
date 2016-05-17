;; -*-emacs-lisp-*-

(if (locate-library "scala-mode2")
    (progn
      ;; See https://github.com/hvesalai/sbt-mode for more
      ;;
      ;; Basic Usage
      ;; 1. sbt-mode needs to find your project home directory.
      ;;    See `sbt:find-root' for how.
      ;; 2. run `sbt-start', with [C-!] in your .scala
      ;; 3. In the *sbt* buffer, run "console"
      ;; 4. In your .scala buffer, use [C-r] to send the current region
      ;;    to the Scala interpreter.
      ;;
      ;; To compile the source
      ;; 1. If you entered the Scala interpreter in sbt buffer,
      ;;    Control-D to leave the interpreter
      ;; 2. [C-x '] to compile your source.
      (setq scala-indent:indent-value-expression t
            scala-indent:align-parameters t
            scala-indent:align-forms t)

      (defun sbt-compile (&optional arg)
        (interactive "P")
        (if prefix-arg
            (call-interactively 'compile)
          (sbt-command "test:compile")))

      (add-hook 'sbt-mode-hook
                '(lambda ()
                   ;; compilation-skip-threshold tells the compilation
                   ;; minor-mode which type of compiler output can be
                   ;; skipped. 1 = skip info 2 = skip info and
                   ;; warnings.
                   (setq compilation-skip-threshold 1)

                   ;; Bind C-a to 'comint-bol when in sbt-mode. This
                   ;; will move the cursor to just after prompt.
                   (local-set-key (kbd "C-a") 'comint-bol)

                   ;; Bind M-RET to 'comint-accumulate. This will
                   ;; allow you to add more than one line to scala
                   ;; console prompt before sending it for
                   ;; interpretation. It will keep your command
                   ;; history cleaner.
                   (local-set-key (kbd "M-RET") 'comint-accumulate)))

      (add-hook 'scala-mode-hook
                '(lambda ()
                   ;; sbt-find-definitions is a command that tries to
                   ;; find (with grep) the definition of the thing at
                   ;; point.
                   (local-set-key (kbd "M-.") 'sbt-find-definitions)

                   ;; use sbt-run-previous-command to re-compile your
                   ;; code after changes
                   (local-set-key (kbd "C-x '") 'sbt-run-previous-command)
                   (local-set-key [(control ?c) ?!] 'run-scala)
                   (local-set-key [(control ?c) ?c] 'sbt-compile)
                   (local-set-key [(control ?c) (control ?r)]
                                  'sbt-paste-region))))

  ;; Currently, Scala 2.8.x is not provided by gentoo portage. Thus, I
  ;; will use the binary distribution from the Scala repository in
  ;; /opt/scala
  (when (not (locate-library "scala-mode-auto"))
    (let* ((scala-mode-path "/opt/scala/misc/scala-tool-support/emacs")
           (scala-file (concat (file-name-as-directory scala-mode-path)
                               "scala-mode-auto.el")))
      (if (file-exists-p scala-file)
          (add-to-list 'load-path scala-mode-path))))


  (with-eval-after-load "scala-mode"
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
    )

  (when (locate-library "scala-mode-auto")
    (require 'scala-mode-auto)))
