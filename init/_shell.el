;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'transient)
(require 'cinsk-common)

;;
;; Make the inferior shell a login shell.
;;
(setq explicit-bash-args '("--noediting" "-i" "-l"))

;; `shell' runs an inferior shell in ASCII coding system.
;; `unicode-shell' behaves the same as `shell' except it runs an inferior
;; shell in UTF-8 coding system.
(defun unicode-shell (&optional encoding)
  "Execute the shell buffer in UTF-8 encoding.
Note that you'll need to set the environment variable LANG and others
appropriately."
  (interactive)
  (let ((coding-system-for-read (or encoding 'utf-8))
        (coding-system-for-write (or encoding 'utf-8))
        (coding-system-require-warning t))
    (call-interactively 'shell)))

;; Allow shell mode to handle color output from shell commands
;; (notably from ls --color command)
;;
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


(keymap-global-set "C-c d" #'shell)


;; (require 'em-smart)
;; (setq eshell-where-to-jump 'begin)
;; (setq eshell-review-quick-commands nil)
;; (setq eshell-smart-space-goes-to-end t)
(setq shell-pushd-regexp "\\(pushd\\|c\\)"
      shell-popd-regexp "\\(popd\\|p\\)")

;;
;; From NEWS Emacs version 25
;;
;; When you invoke 'shell' interactively, the '*shell*' buffer will now
;; display in a new window.  However, you can customize this behavior via
;; the 'display-buffer-alist' variable.  For example, to get
;; the old behavior -- '*shell*' buffer displays in current window -- use
;; (add-to-list 'display-buffer-alist
;;      '("^\\*shell\\*$" . (display-buffer-same-window))).
(add-to-list 'display-buffer-alist
             '("^\\*shell\\*$" . (display-buffer-same-window)))

(with-eval-after-load "sh-script"
  ;; sh-mode uses C-c C-o and C-c C-c for its own purpose, which
  ;; prevents me to use them for a global binding.
  (my/move-key :keymap sh-mode-map :from "C-c C-o" :to "C-c C-S-o")
  (my/move-key :keymap sh-mode-map :from "C-c C-c" :to "C-c C-S-c"))


(defun my/ghostel-switch-to-last (&optional create)
  "Switch to the most recently used ghostel buffer.

If the current buffer is a ghostel buffer, skip it, so repeated
calls bounce between the two most recent ons."
  (interactive)
  (let ((target (seq-find (lambda (buf)
                            (and (not (eq buf (current-buffer)))
                                 (provided-mode-derived-p (buffer-local-value 'major-mode buf)
                                                          'ghostel-mode)))
                          (buffer-list))))
    (if target
        (pop-to-buffer target
                       (append display-buffer--same-window-action '((category . comint))))
      (if create
        (user-error "no other ghostel buffer")))))

(defun my/ghostel-emacs-mode-and-scroll-down (&optional arg)
"Switch to ghostel emacs mode, then scroll back like `M-v'."
  (interactive "^P")
  (ghostel-emacs-mode)
  (scroll-down-command arg))

(defun my/ghostel-new ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'ghostel)))

(defvar-keymap my/ghostel-screen-map
  :doc "tmux/screen style prefix map for ghostel buffers."
  "\""  #'ghostel-list-buffers
  "a"   #'ghostel-send-C-z
  "A"   #'rename-buffer
  "c"   #'my/ghostel-new
  "C"   #'ghostel-project
  "n"   #'ghostel-next
  "p"   #'ghostel-previous
  "C-z" #'my/ghostel-switch-to-last)

(transient-define-prefix my/ghostel-menu ()
  "Ghostel menu"
  ["Actions"
   [ ("\"" "List all buffers" ghostel-list-buffers)
     ("n"  "Switch to Next shell" ghostel-next)
     ("p"  "Switch to Previous shell" ghostel-previous)
     ("C-z" "Switch to the last shell" my/ghostel-switch-to-last) ]
   [ ("c"  "New shell" my/ghostel-new)
     ("C"  "New shell for a project" ghostel-project) ]
   [ ("a"  "Send escape code"  ghostel-send-C-z)
     ("A"  "Rename buffer" rename-buffer) ]])

(use-package ghostel
  :ensure t
  :hook (ghostel-mode . ghostel-ime-mode)
  :config
  (my/move-key :from "C-x m" :to "C-x M" :with #'ghostel)
  (when (not (eq system-type 'darwin))
    ;; On macOS, ghostel seems to make the shell a login shell by default.
    (setq ghostel-shell '("/bin/bash" "--login")))

  (keymap-set ghostel-semi-char-mode-map "M-v" #'my/ghostel-emacs-mode-and-scroll-down)

  ;; Free some key bindings in the semi-char-modes that would
  ;; otherwise forward it to the terminal, so lookup falls through to
  ;; ghostel-mode-map or the global map
  (keymap-unset ghostel-semi-char-mode-map "C-z" t) ; my/ghostel-menu
  (keymap-unset ghostel-semi-char-mode-map "C-<tab>" t)

  ;; Below code will unset all key bindings that has function-key
  ;; (e.g. "C-<f3>") in it.  The intension is to pass all function-key
  ;; bindings to the parent map.
  (map-keymap-internal
   (lambda (e b)
     (let ((kd (key-description (vector e))))
       (when (and (string-match "<f[^>]*>" kd)
                  (eq b 'ghostel--send-event))
         (keymap-unset ghostel-semi-char-mode-map kd t))))
   ghostel-semi-char-mode-map)

  ;; Free C-z in the modes that would otherwise forward it to the terminal,
  ;; so lookup falls through to ghostel-mode-map.
  (dolist (map (list ghostel-semi-char-mode-map ghostel-char-mode-map))
    (keymap-set map "M-v" #'my/ghostel-emacs-mode-and-scroll-down)
    (keymap-unset map "C-<tab>" t)
    (keymap-unset map "C-z" t))

  (if (fboundp 'my/ghostel-menu)
      (keymap-set ghostel-mode-map "C-z" #'my/ghostel-menu)
    (keymap-set ghostel-mode-map "C-z" my/ghostel-screen-map))

  ;; Whitelisted Emacs functions accessible from ghostel_cmd.  See
  ;; ../ghostel.bash for more
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer))
  (add-to-list 'ghostel-eval-cmds '("calc" calc))
  (add-to-list 'ghostel-eval-cmds '("ediff-files" ediff-files))
  (add-to-list 'ghostel-eval-cmds '("ediff-files3" ediff-files3))
  )


(use-package consult-ghostel
  :if (locate-library "consult-ghostel")  ; skip entirely if not installed
  :ensure nil                             ; only needed if always-ensure is on
  :after ghostel
  :demand t                               ; enable mode as soon as deps load
  :config
  (consult-ghostel-mode)
  (transient-replace-suffix 'my/ghostel-menu "\""
    '("\"" "List all buffers" consult-ghostel))
  :bind
  (("C-x m" . consult-ghostel)
   :map project-prefix-map
   ("m" . consult-ghostel-project)
   :map ghostel-semi-char-mode-map
   ("C-c h" . consult-ghostel-history)))
