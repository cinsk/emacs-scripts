;; -*-emacs-lisp-*-

;;;
;;; Go configuration
;;;


;; (add-hook 'go-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook 'gofmt-before-save)
;;             (setq indent-tabs-mode 1)))

(when (locate-library "subword")
  (add-hook 'go-mode-hook
            (lambda ()
              (subword-mode 1)
              (cinsk/go-init-ac-autocomplete))))

(defun cinsk/go-init-ac-autocomplete ()
  "Set up auto completion of Go sources.

See https://github.com/auto-complete/auto-complete for more"
  ;; go-autocomplete.el is distributed with gocode.
  ;; It seems that I need to manually add gocode into the workspace.
  ;; The workspace location is specified in the environment, GOPATH.
  (unless (locate-library "go-autocomplete")
    (let ((go-ac-dir (substitute-in-file-name
                       "$GOPATH/src/github.com/nsf/gocode/emacs")))
      (when (locate-library "go-autocomplete" nil (list go-ac-dir))
        (add-to-list 'load-path go-ac-dir)

        (unless (executable-find "gocode")
          (add-to-list 'exec-path (substitute-in-file-name "$GOPATH/bin"))
          (unless (executable-find "gocode")
            (warn "gocode not found; try build it using 'go install github.com/nsf/gocode' in $GOPATH directory")))
                 
        (unless (featurep 'go-autocomplete)
          (require 'go-autocomplete)
          (require 'auto-complete-config)
          (ac-config-default)
          (when (boundp 'go-mode-map)
            ;; A key binding to force complete (show completion candidate).
            ;; I thought `ac-complete' would call `ac-complete-go', but
            ;; `ac-complete' has no effect.
            (define-key go-mode-map [(meta ?I)] 'ac-complete-go)))))))


      
