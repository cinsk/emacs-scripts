;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;
;;; Buffer Menu (buffer-menu or ibuffer) configuration
;;;

(eval-when-compile
  (require 'ibuf-ext))

;; Sort by the 2nd column (buffer name) in Buffer list
(setq Buffer-menu-sort-column 2)

;; ibuffer - advanced buffer menu
(keymap-global-set "C-x C-b" #'ibuffer)
;; (autoload 'ibuffer "ibuffer" "List buffers." t)

;; I want ibuffer shows all buffer including internal one as I use
;; ibuffer mostly for debugging.
(setq ibuffer-maybe-show-predicates nil)

(setq ibuffer-saved-filter-groups
      '(("default"
         ("dired" (mode . dired-mode))
         ("manual" (or
                    (name . "^\\*info.*\\*$")
                    (name . "^\\*Man.*\\*$")
                    (name . "^\\*Help.*\\*$")))
         ("joplin" (or
                    (mode . joplin-search-mode)
                    (mode . joplin-mode)
                    (name . "^\\*[jJ]oplin.*$")))
         ("elisp" (or
                   (mode . emacs-lisp-mode)
                   (name . "\\`\\*scratch\\*\\'")))
         ("gnus" (or
                  (name . "\\`\\*Group\\*\\'")
                  (name . "\\`\\*Server\\*\\'")
                  (name . "\\`\\*Article .*\\*\\'")
                  (name . "\\`\\*Summary .*\\*\\'")))
         ("private" (or
                      (name . "^TAGS$")
                      (name . "^\\*.*\\*$")))
         ("internal" (or
                      (name . "^ .*$")))
         )))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
