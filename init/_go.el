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
              ;; Surprisingly, `c-basic-offset' has no effect on
              ;; go-mode, but `tab-width' does.
              (setq tab-width 4)
              (subword-mode 1))))

(add-hook 'go-mode-hook 'cinsk/go-check-utils)
(add-hook 'go-mode-hook 'lsp-deferred)

(defun cinsk/go-check-utils (&optional nowarn)
  (if (executable-find "gopls")
      (progn
        (add-hook 'before-save-hook #'lsp-format-buffer t t)
        (add-hook 'before-save-hook #'lsp-organize-imports t t))
      (unless nowarn
        (warn "gopls not found; See https://github.com/golang/tools/tree/master/gopls for more"))))

(when nil
  (with-eval-after-load "go-mode"
    (add-hook 'before-save-hook 'gofmt-before-save))

  ;;
  ;; Prefer company-mode based auto-completion to auto-complete-mode based one.
  ;;
  (cond ((locate-library "company")
         (add-hook 'go-mode-hook 'cinsk/go-init-cp-autocomplete))
        ((locate-library "auto-complete")
         (add-hook 'go-mode-hook 'cinsk/go-init-ac-autocomplete)))

  (defun cinsk/go-check-gocode (&optional nowarn)
    (unless (executable-find "gocode")
      (add-to-list 'exec-path (substitute-in-file-name "$GOPATH/bin"))
      (unless (and (executable-find "gocode") nowarn)
        (warn "gocode not found; try build it using 'go get github.com/nsf/gocode' in $GOPATH directory"))))

  (defun cinsk/go-check-utils (&optional nowarn)
    (if (executable-find "goimports")
        (setq gofmt-command "goimports")
      (add-to-list 'exec-path (substitute-in-file-name "$GOPATH/bin"))

      (if (executable-find "goimports")
          (setq gofmt-command "goimports")
        (unless nowarn
          (warn "goimports not found; try build it using 'go get golang.org/x/tools/cmd/goimports' in $GOPATH directory")))))

  (defun cinsk/go-init-cp-autocomplete-deprecated ()
    "Set up auto completion of Go sources using `company-mode'"
    (unless (locate-library "company-go")
      (let ((go-cp-dir (substitute-in-file-name
                        "$GOPATH/src/github.com/nsf/gocode/emacs-company")))
        (when (locate-library "company-go" nil (list go-cp-dir))
          (add-to-list 'load-path go-cp-dir)

          (cinsk/go-check-gocode)

          (unless (featurep 'company-go)
            (require 'company)
            (require 'company-go)

            ))))
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode))

  (defun cinsk/go-init-ac-autocomplete-deprecated ()
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

          (cinsk/go-check-gocode)

          (unless (featurep 'go-autocomplete)
            (require 'go-autocomplete)
            (require 'auto-complete-config)
            (ac-config-default)
            (when (boundp 'go-mode-map)
              ;; A key binding to force complete (show completion candidate).
              ;; I thought `ac-complete' would call `ac-complete-go', but
              ;; `ac-complete' has no effect.
              (define-key go-mode-map [(control meta ?I)] 'ac-complete-go)))))))
)
