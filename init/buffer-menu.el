;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;
;;; Buffer Menu (buffer-menu or ibuffer) configuration
;;;

(eval-when-compile
  (require 'ibuf-ext))

(defun my/ibuffer-toggle-internal ()
  "Togle display of hidden/internal buffers in ibuffer."
  (interactive)
  (ibuffer-update t))

(defun my/ibuffer-flat (&optional reverse)
  "Flat buffer list with no groups, sorted by recency.
Most recently used first; with prefix arg REVERSE, oldest first."
  (interactive "P")
  (setq ibuffer-filter-groups nil
	ibuffer-sorting-mode 'recency
	ibuffer-sorting-reversep (and reverse t))
  (ibuffer-update nil t))

(defun my/ibuffer-grouped (&optional name)
  "Group buffers using the saved filter groups called NAME.
If NAME is nil, use \"default\".  Interactively, a prefix arg prompts
for the name."
  (interactive (list (when current-prefix-arg
		       (completing-read "Filter group: "
					(mapcar #'car ibuffer-saved-filter-groups)
					nil t))))
  (ibuffer-switch-to-saved-filter-groups (or name "default")))

(setq ibuffer-saved-filters '(("dired" (derived-mode . dired-mode))
			      ("code" (derived-mode . prog-mode))
			      ("text" (derived-mode . text-mode))
			      ("process" (predicate . (get-buffer-process (current-buffer))))
			      ("doc" (or (mode . Info-mode)
					 (mode . Man-mode)
					 (mode . woman-mode)
					 (mode . help-mode)
					 (mode . eldoc-mode)
					 (mode . shortdoc-mode)))
			      ("shell" (or (mode . shell-mode)
					   (mode . eshell-mode)
					   (mode . term-mode)
					   (mode . ghostel-mode)
					   (mode . vterm-mode)
					   (mode . eat-mode)))
			      ("vc" (or (derived-mode . magit-mode)
					(derived-mode . vc-dir-mode)
					(derived-mode . log-view-mode)
					(derived-mode . log-edit-mode)
					(mode . vc-annotate-mode)
					(name . "^\\*vc")
					(filename . "/\\.git/COMMIT_EDITMSG$")))
			      ("joplin" (or (mode . joplin-search-mode)
					    (mode . joplin-mode)
					    (name . "^\\*[jJ]oplin.*$")))
			      ("gnus" (or (name . "\\`\\*Group\\*\\'")
					  (name . "\\`\\*Server\\*\\'")
					  (name . "\\`\\*Article .*\\*\\'")
					  (name . "\\`\\*Summary .*\\*\\'")))
			      ("private" (or (name . "^TAGS$")
					     (name . "^\\*.*\\*$")))
			      ("internal" (name . "^ .*$"))))

(setq ibuffer-saved-filter-groups
      '(("default"
	 ("Code" (and (not (saved . "internal")) (saved . "code")))
	 ("Text" (and (not (saved . "internal")) (saved . "text")))
	 ("Dired" (saved . "dired"))
	 ("Shell" (and (not (saved . "internal")) (saved . "shell")))
	 ("Ref" (saved . "doc"))
	 ("VC" (and (not (saved . "internal")) (saved . "vc")))
	 ("Joplin" (saved . "joplin"))
	 ("Private" (saved . "private"))
	 ("Internal" (saved . "internal")))))


(keymap-global-set "C-x C-b" #'ibuffer)

(setq ibuffer-show-empty-filter-groups nil)

(with-eval-after-load 'ibuffer
  (keymap-set ibuffer-mode-map "i" #'my/ibuffer-toggle-internal)
  (keymap-set ibuffer-mode-map "[" #'my/ibuffer-grouped)
  (keymap-set ibuffer-mode-map "]" #'my/ibuffer-flat))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")
	    (setq ibuffer-hidden-filter-groups '("Internal"))
	    (ibuffer-update nil t)))
