;;; early-init.el --- -*- lexical-binding: t; -*-

;; See also https://specifications.freedesktop.org/basedir/latest/
(defun my/xdg (var default)
  "Return the emacs/ subdirectory of XDG variable VAR, falling back to DEFAULT."
  (let ((v (getenv var)))
    (file-name-as-directory
     (expand-file-name
      "emacs"
      (if (and v (not (string-empty-p v))) v (expand-file-name default "~"))))))

(defconst my/cache-dir  (my/xdg "XDG_CACHE_HOME"  ".cache"))
(defconst my/state-dir  (my/xdg "XDG_STATE_HOME"  ".local/state"))
(defconst my/data-dir   (my/xdg "XDG_DATA_HOME"   ".local/share"))
(defconst my/config-dir (my/xdg "XDG_CONFIG_HOME" ".config"))

(defun my/cache-file  (n) (expand-file-name n my/cache-dir))
(defun my/state-file  (n) (expand-file-name n my/state-dir))
(defun my/data-file   (n) (expand-file-name n my/data-dir))
(defun my/config-file (n) (expand-file-name n my/config-dir))

(dolist (d (list (my/cache-file "")
                 (my/state-file "backups/") (my/state-file "auto-save/")
                 (my/state-file "auto-save-list/") (my/state-file "tramp-autosave/")
                 (my/state-file "transient/") (my/state-file "url/")
                 (my/data-file "") (my/config-file "")))
  (make-directory d t))

;; ---- Cache: disposable ----
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (my/cache-file "eln-cache/")))

(setq tramp-persistency-file-name (my/cache-file "tramp")
      org-persist-directory       (my/cache-file "org-persist/")
      package-quickstart-file     (my/cache-file "package-quickstart.el"))

;; ---- State: history, recovery, session ----
(setq backup-directory-alist         `(("." . ,(my/state-file "backups/")))
      auto-save-file-name-transforms `((".*" ,(my/state-file "auto-save/") t))
      lock-file-name-transforms      `((".*" ,(my/state-file "auto-save/") t))
      auto-save-list-file-prefix     (my/state-file "auto-save-list/.saves-")
      tramp-auto-save-directory      (my/state-file "tramp-autosave/")
      savehist-file                  (my/state-file "history")
      recentf-save-file              (my/state-file "recentf")
      save-place-file                (my/state-file "places")
      project-list-file              (my/state-file "projects")
      ido-save-directory-list-file   (my/state-file "ido.last")
      eshell-directory-name          (my/state-file "eshell/")
      transient-history-file         (my/state-file "transient/history.el")
      transient-levels-file          (my/state-file "transient/levels.el")
      transient-values-file          (my/state-file "transient/values.el")
      org-id-locations-file          (my/state-file "org-id-locations")
      org-clock-persist-file         (my/state-file "org-clock-save.el")
      lsp-session-file               (my/state-file "lsp-session-v1")
      dap-breakpoints-file           (my/state-file "dap-breakpoints")
      nsm-settings-file              (my/state-file "network-security.data")
      url-configuration-directory    (my/state-file "url/")
      url-cache-directory            (my/cache-file "url/"))

;; ---- Data: durable ----
;; (setq package-user-dir      (my/data-file "elpa/")
(setq multisession-directory (my/data-file "multisession/")
      bookmark-default-file  (my/data-file "bookmarks"))

;; ---- Config: hand-edited ----
(setq custom-file             (my/config-file "custom.el")
      calc-settings-file      (my/config-file "calc.el")
      auto-insert-directory   (my/config-file "insert/")
      magit-user-githook-file (my/config-file "magit-githooks"))

