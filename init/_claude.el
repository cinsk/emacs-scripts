;; -*- mode: emacs-lisp; lexical-binding: t; -*-


(defun my/claude-buffer-p (buf)
  "Non-nil if BUF is a Claude Code console buffer."
  (string-match-p "\\`\\*claude-code\\[" (buffer-name buf)))

(defun my/claude-buffer-for-directory (dir)
  "Return the Claude buffer whose root directory contains DIR.
If several match (e.g. nested projects), prefer the deepest root."
  (let ((dir (file-truename dir))
        best best-len)
    (dolist (buf (buffer-list) best)
      (when (my/claude-buffer-p buf)
        (let ((root (file-truename
                     (buffer-local-value 'default-directory buf))))
          (when (and (file-in-directory-p dir root)
                     (or (null best) (> (length root) best-len)))
            (setq best buf
                  best-len (length root))))))))

(defun my/claude-raise-or-list ()
  "Hide Claude if it's the current buffer; otherwise raise the Claude
console for this directory, or list sessions if there is none."
  (interactive)
  (if (my/claude-buffer-p (current-buffer))
      (quit-window)
    (let ((buf (my/claude-buffer-for-directory default-directory)))
      (if buf
          (pop-to-buffer buf)
        (claude-code-ide-list-sessions)))))

(use-package ghostel-ime
  :hook (ghostel-mode . ghostel-ime-mode))

;; install claude-code.el
(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (global-set-key [(control meta ?`)] #'my/claude-raise-or-list)
  ;; (claude-code-ide-emacs-tools-setup) ; Optionally enable Emacs MCP tools
  )


;; (keymap-global-set "C-c c" #'my/claude-raise-or-list)
