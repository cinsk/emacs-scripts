;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq enable-recursive-minibuffers t
      completion-cycle-threshold 3
      tab-always-indent 'complete
      read-extended-command-predicate #'command-completion-default-include-p
      minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; --- Persist minibuffer history (so recent commands sort first) ---
(use-package savehist
  :ensure nil
  :init (savehist-mode 1))

;;; --- Vertico: the vertical completion UI ---
(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("C-j" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)
              ("C-l" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)

  :init (vertico-mode)

  :custom
  (vertico-cycle t)
  (vertico-count 15)
  (vertico-resize nil))


;; Resume the last completion session (like helm-resume)
(use-package vertico-repeat
  :ensure nil                           ; ships with the vertico package
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (("C-c r" . vertico-repeat)))


;;; --- Orderless: space-separated, any-order matching (Helm-style) ---
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  ;; Keep partial-completion for files so `/u/l/b` style paths still work
  (completion-category-overrides '((file (styles partial-completion)))))


;;; --- Marginalia: annotations (docstrings, file sizes, keybindings...) ---
(use-package marginalia
  :ensure t
  :init (marginalia-mode 1)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))


;;; --- Consult: the Helm-command replacements ---
(use-package consult
  :ensure t
  :bind
  (;; C-c bindings
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ;; Buffers / files
   ("C-x b"   . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x p b" . consult-project-buffer)
   ("C-x r b" . consult-bookmark)
   ;; Editing
   ("M-y" . consult-yank-pop)
   ;; Navigation
   ("M-g g"   . consult-goto-line)
   ("M-g i"   . consult-imenu)
   ("M-g I"   . consult-imenu-multi)
   ("M-g o"   . consult-outline)
   ("M-g m"   . consult-mark)
   ("C-S-s"   . consult-line)
   ("M-s l"   . consult-line)
   ("M-s L"   . consult-line-multi)
   ;; Search
   ("M-s r"   . consult-ripgrep)
   ("M-s g"   . consult-grep)
   ("M-s d"   . consult-find)
   ("M-s f"   . consult-fd)
   ;; Isearch integration
   :map isearch-mode-map
   ("M-e"   . consult-isearch-history)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi))
  :init
  ;; Use Consult for xref (definitions/references)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Better register preview
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  ;; Delay preview for expensive sources
  (consult-customize
   consult-ripgrep consult-grep consult-git-grep consult-bookmark
   consult-recent-file consult-source-recent-file
   consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

;;; --- Embark: actions on candidates (replaces Helm's action menu) ---
(use-package embark
  :ensure t
  :bind
  (("C-."   . embark-act)           ; like helm's C-z
   ("C-;"   . embark-dwim)
   ("C-h B" . embark-bindings))     ; replaces helm-descbinds
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffer
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; --- Optional: editable grep results (embark-export on consult-ripgrep) ---
(use-package wgrep
  :disabled
  :ensure t)

;;; --- Optional: in-buffer completion (replaces company) ---
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

  ;; (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  ;; corfu-auto-trigger "." ;; Custom trigger characters
  :init (global-corfu-mode 1))


(use-package eglot
  :ensure nil                                   ; built in (Emacs 29+)
  :hook ((python-mode    . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (go-mode        . eglot-ensure)
         (go-ts-mode     . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format)
              ("C-c l F" . eglot-format-buffer)
              ("C-c l o" . eglot-code-action-organize-imports)
              ("C-c l i" . eglot-find-implementation)
              ("C-c l t" . eglot-find-typeDefinition)
              ("C-c l h" . eldoc-doc-buffer)
              ("C-c l R" . eglot-reconnect))
  :custom
  (eglot-autoshutdown t)                        ; kill server when last buffer closes
  (eglot-sync-connect nil)                      ; don't block Emacs while the server starts
  (eglot-extend-to-xref t)                      ; keep Eglot active in xref-visited files
  (eglot-send-changes-idle-time 0.3)
  (eglot-events-buffer-config '(:size 0 :format full)) ; Emacs 30+. Older: (eglot-events-buffer-size 0)
  :config
  ;; --- Server selection ---
  ;; Emacs 29+: first installed alternative wins.
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode)
                 . ,(eglot-alternatives
                     '(("basedpyright-langserver" "--stdio")
                       ("pyright-langserver" "--stdio")
                       "pylsp"
                       "jedi-language-server"
                       ("ruff" "server")))))
  ;; gopls is already in the default eglot-server-programs, no entry needed.

  ;; --- Per-server settings (sent as workspace/didChangeConfiguration) ---
  (setq-default eglot-workspace-configuration
                '(:gopls (:staticcheck t
                          :gofumpt t
                          :usePlaceholders t
                          :completeUnimported t
                          :analyses (:unusedparams t :shadow t)
                          :hints (:parameterNames t :assignVariableTypes t))
                  :basedpyright.analysis (:typeCheckingMode "standard"
                                          :autoImportCompletions t
                                          :diagnosticMode "openFilesOnly")
                  :python.analysis (:typeCheckingMode "basic")))   ; used if you run plain pyright

  ;; Nice-to-have: inlay hints (types, param names) on Emacs 29.1+
  (add-hook 'eglot-managed-mode-hook
            (lambda () (when (eglot-server-capable :inlayHintProvider)
                         (eglot-inlay-hints-mode 1))))

  ;; Eglot’s default completion style is flex for its own
  ;; category. Because earlier we set completion-category-defaults to
  ;; nil with global orderless, Eglot candidates will use
  ;; Orderless. If you want to be explicit, add this (the category is
  ;; eglot-capf on Emacs 30, eglot on 29):
  (setq completion-category-overrides
        '((file (styles partial-completion))
          (eglot (styles orderless basic))
          (eglot-capf (styles orderless basic))))
  )

(defun my/eglot-setup-capf ()
  "Merge Eglot with dabbrev and keep file completion."
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     (cape-capf-buster #'eglot-completion-at-point)
                     #'cape-dabbrev)
                    #'cape-file)))


(use-package cape
  :ensure t
  :hook (eglot-managed-mode . my/eglot-setup-capf)

  :init
  ;; Global fallbacks for buffers without Eglot
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))


;;; --- Optional: icons ---
(use-package nerd-icons-completion
  :disabled
  :ensure t
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init (nerd-icons-completion-mode 1))
