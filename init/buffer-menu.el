;; -*-emacs-lisp-*-

;;;
;;; Buffer Menu (buffer-menu or ibuffer) configuration
;;;

(eval-when-compile
  (require 'ibuf-ext))

;; Sort by the 2nd column (buffer name) in Buffer list
(setq Buffer-menu-sort-column 2)

;; ibuffer - advanced buffer menu
(global-set-key [(control ?x) (control ?b)] 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

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
         ("internal" (or
                      (name . "^TAGS$")
                      (name . "^\\*.*\\*$"))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
