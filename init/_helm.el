;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;
;;; Helm configuration
;;;

(require 'cinsk-common)

(when (locate-library "helm")
  (require 'helm)
  ; (require 'helm-config) -- it seems helm-config is removed
  (helm-mode))

(my/move-key :from "M-x" :to "M-X" :with #'helm-M-x :overwrite t)
(my/move-key :from "C-x b" :to "C-x B" :with #'helm-buffers-list)
(my/move-key :from "C-x C-f" :to "C-x C-F" :with #'helm-find-files)
(my/move-key :from "M-y" :to "M-Y" :with #'helm-show-kill-ring)

(keymap-global-set "C-x C-a" #'helm-command-prefix)


;; See http://tuhdo.github.io/helm-intro.html

;; rebind tab to do persistent action
(keymap-set helm-map "<tab>" #'helm-execute-persistent-action)
;; make TAB works in terminal
(keymap-set helm-map "TAB" #'helm-execute-persistent-action)

;; list actions using C-z
(keymap-set helm-map "C-z" #'helm-select-action)

(keymap-set helm-map "M-m" #'helm-toggle-visible-mark)
;(keymap-set helm-buffer-map "M-m" #'helm-toggle-visible-mark)

(when (executable-find "curl")
  (if (boundp 'helm-net-prefer-curl)
      (setq helm-net-prefer-curl t)
    (setq helm-net-prefer-curl t)))


(keymap-set helm-command-map "`" #'helm-resume)
(keymap-set helm-command-map "R" #'helm-regexp)

(defun helm-recursive-grep ()
  "Call `helm-do-grep' with a prefix argument given."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'helm-do-grep)))

(keymap-set helm-command-map "r" #'helm-recursive-grep)
(keymap-set helm-command-map "g" #'helm-occur)

(setq
 ;; open helm buffer inside current window, not occupying whole other
 ;; window
 ;;
 ;; helm-split-window-in-side-p t

 ;; move to end or beginning of source when reaching top or
 ;; bottom of source.
 ;;
 helm-move-to-line-cycle-in-source nil

 ;; search for library in `require' and `declare-function'
 ;; sexp.
 helm-ff-search-library-in-sexp t

 ;; scroll 8 lines other window using M-<next>/M-<prior>
 helm-scroll-amount 8

 helm-ff-file-name-history-use-recentf t)


;; base16-theme-3.2 causes helm's completing-read failed: stringp or
;; nil I can't figure it out why it failed; it seems that
;; `custom-available-themes' generate invalid value for the
;; `completing-read', but when I tried manually, there is nothing
;; wrong about it.  Since I don't call `load-theme' frequently, I'm
;; removing it from the helm.
(add-to-list 'helm-completing-read-handlers-alist '(load-theme . nil))
