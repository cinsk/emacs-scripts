


;;; The default value of `gc-cons-threshold' is set to 800000 on
;;; 64-bit system.

(defvar cinsk/gc-cons-threshold (* 10 gc-cons-threshold))
(setq gc-cons-threshold cinsk/gc-cons-threshold)
;;(setq garbage-collection-messages t)

;;
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;;

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold cinsk/gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook 'append)


;;;
;;; ICE setup
;;;
(add-to-list 'auto-mode-alist '(".*\\.ice$" . java-mode))


(defmacro time-loop (n &rest body)
  "Return time before and after N iteration of BODY.

DO NOT USE THIS MACRO.  INSTEAD, USE `benchmark'."
  (declare (indent 1) (debug t))
  `(let ((t1 (current-time)))
     (dotimes (i ,n)
       ,@body)
     (time-subtract (current-time) t1)))


(defmacro save-font-excursion (face &rest body)
  "Save the :font property of given FACE during the execution of BODY."
  (declare (indent 1) (debug t))
  (let ((face-sym (gensym "face-"))
        (oldfont (gensym "oldfont-"))
        (ret (gensym "ret-")))
    `(let* ((,face-sym ,face)
            (,oldfont (face-attribute ,face-sym :font))
            ,ret)
       (setq ,ret (progn ,@body))
       (or (string= ,oldfont (face-attribute ,face-sym :font))
           (set-face-attribute ,face-sym nil :font ,oldfont))
       ,ret)))


(defun path-join (path &rest args)
  "Join all parameters to build a pathname."
  (if args
      (apply 'path-join
             (concat (file-name-as-directory path) (car args))
             (cdr args))))


(defun fkey (arg)
  "Send a function key press event to Emacs.

Without a prefix argument, it simulate <f1> key press.
A prefix argument N is translated to <fn> key"
  (interactive "p")
  (if (<= arg 0)
      (error "prefix argument between 1 to 12 is required"))
  (let ((ks (kbd (format "<f%d>" arg))))
         ;(vector (list (intern (format "f%d" arg))))))
    (message "key: %S" ks)
    (add-to-list 'unread-command-events (listify-key-sequence ks))))


(with-eval-after-load "tls"
  ;; On recent Mac (Mac Mojave), the ca-bundle is not available from
  ;; the file system, so `gnutls-trustfiles' would return nil. If the
  ;; `tls-program' uses "--x509cafile nil" command-line, gnutls-cli
  ;; will fail, so I'm going to remove "--x509cafile" from the
  ;; command-line
  ;;
  ;; TODO: Is it secure enough?
  ;; To verify, run:
  ;;    (url-copy-file "https://www.google.com/" "google.txt")
  (add-to-list 'tls-program "gnutls-cli -p %p %h"))




;;;
;;; Switching between buffers using iswitchb/ido
;;;
(when nil
  (unless (package-installed-p 'helm)
    (use-package iswitchb
      :disabled
      :config
      ;; (iswitchb-mode 1)                     ; smart buffer switching mode
      (setq iswitchb-default-method 'maybe-frame)) ; ask to use another frame.

    ;; browse-kill-ring: interactive kill-ring navigation
    ;; disabled in favor of `helm-show-kill-ring'
    ;;
    ;; (when (uinit-require 'browse-kill-ring)
    ;;   (when (fboundp 'browse-kill-ring)
    ;;     (keymap-global-set "C-c C-k" #'browse-kill-ring)))

    (use-package ido
      :config
      (ido-mode 1)

      ;;(setq ido-default-buffer-method 'maybe-frame)
      (setq ido-use-filename-at-point 'guess)
      (setq ido-max-directory-size 100000)

      (when (boundp 'ido-file-completion-map)
        ;; The default binding `C-x C-f' is too much for me
        (keymap-set ido-file-completion-map "C-<return>" #'ido-enter-dired)
        (keymap-set ido-file-completion-map "M-<return>" #'ido-enter-dired)))))

;; I tried helm-mode, which looks promising, but it does not seem to
;; have enough short-cut mechanism to boost user's input.  For
;; example, completing q-r-r is not possible for
;; `query-replace-regexp'.  And if there are multiple choices, I need
;; to navigate using up/down shortcut, with careful on-looking to the
;; helm buffer.  It distract me from what I try to do.  Until helm
;; updated, or until I found better way, I'll stick to
;; 'icomplete-mode.
;(icomplete-mode 1)
;(with-eval-after-load "icomplete"
;  (uinit-require 'icomplete+ nil 'noerror))

;; Now I'm using helm.  One minor problem is, whenever I invoke
;; `helm-find-files', sometimes it freezes.  I believe this is related
;; to the garbage collection. See the comment in the beginning of the
;; file for `garbage-collection-messages'.

;;
;; Normally, switching buffer commands (e.g. `switch-to-buffer' or
;; `iswitchb-buffer') ignore buffer that matched "^ ".
;;
;; When I develop some lisp package, I need to switch to buffers that
;; begins with "^ ".  Thus, I don't use "^ " as the value of
;; `iswitchb-buffer-ignore'.
;;
;; The problem is, sometimes " *Minibuf-1*" buffer will be in the buffer
;; list, which makes me annoying.  Thus, I'll explicitly ignore minibuffer-like
;; names in `iswitchb-buffer-ignore'. -- cinsk
;;
;; (setq iswitchb-buffer-ignore '("^ \\*Minibuf[^*]*\\*"))



(use-package amz-common
  :disabled
  :config
  (when (boundp 'c-default-style)
    ;; Amazon library uses BSD style for C/C++, which I don't like
    (setq c-default-style (delq (assoc 'c++-mode c-default-style)
                                c-default-style)
          c-default-style (delq (assoc 'c-mode c-default-style)
                                c-default-style))))

(use-package amazon-util
  :disabled
  :requires amz-common)

(when (file-directory-p "/apollo/env/EmacsAmazonLibs/share/emacs/site-lisp")
  (add-to-list 'load-path "/apollo/env/EmacsAmazonLibs/share/emacs/site-lisp"))




;;;
;;; htmlize
;;;
(use-package htmlize
  :disabled
  :config
  (setq htmlize-convert-nonascii-to-entities nil))


;;;
;;; vim-modeline
;;;
(let ((vim-modeline-path (expand-file-name
                          (file-name-concat user-emacs-directory
                                            "vim-modeline"))))
  (when (file-accessible-directory-p vim-modeline-path)
    (add-to-list 'load-path vim-modeline-path)
    (when (locate-library "vim-modeline")
      (require 'vim-modeline)
      (add-hook 'find-file-hook 'vim-modeline/do))))

