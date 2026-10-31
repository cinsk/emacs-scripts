;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;; All temporary / cache / state files live under ~/.cache/emacs/
(defconst my/cache-dir (expand-file-name "~/.cache/emacs/")
  "Directory for Emacs temporary, cache and state files.")

(defun my/cache-file (name)
  "Return the absolute path of NAME inside `my/cache-dir'."
  (expand-file-name name my/cache-dir))

(dolist (d '("" "backups/" "auto-save/" "auto-save-list/" "transient/"))
  (make-directory (my/cache-file d) t))

;; Native compilation cache
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (my/cache-file "eln-cache/")))

;; Backups (foo~), auto-saves (#foo#), lock files (.#foo)
(setq backup-directory-alist         `(("." . ,(my/cache-file "backups/")))
      auto-save-file-name-transforms `((".*" ,(my/cache-file "auto-save/") t))
      lock-file-name-transforms      `((".*" ,(my/cache-file "auto-save/") t))
      auto-save-list-file-prefix     (my/cache-file "auto-save-list/.saves-"))

;; State files of built-in and third-party packages
(setq savehist-file                (my/cache-file "history")
      recentf-save-file            (my/cache-file "recentf")
      save-place-file              (my/cache-file "places")
      project-list-file            (my/cache-file "projects")
      tramp-persistency-file-name  (my/cache-file "tramp")
      url-configuration-directory  (my/cache-file "url/")
      nsm-settings-file            (my/cache-file "network-security.data")
      multisession-directory       (my/cache-file "multisession/")
      eshell-directory-name        (my/cache-file "eshell/")
      ido-save-directory-list-file (my/cache-file "ido.last")
      transient-history-file       (my/cache-file "transient/history.el")
      transient-levels-file        (my/cache-file "transient/levels.el")
      transient-values-file        (my/cache-file "transient/values.el")
      org-persist-directory        (my/cache-file "org-persist/")
      org-id-locations-file        (my/cache-file "org-id-locations")
      lsp-session-file             (my/cache-file "lsp-session-v1")
      dap-breakpoints-file         (my/cache-file "dap-breakpoints"))
