;; -*-emacs-lisp-*-

;;;
;;; X window system customization
;;;

(eval-when-compile
  (require 'fontutil nil t))

(setq select-enable-clipboard t)

(when (display-graphic-p)
  (when (locate-library "fontutil")
    (require 'fontutil)
    (fontutil/set-font "scodepro-15")))
