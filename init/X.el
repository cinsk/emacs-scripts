;; -*-emacs-lisp-*-

;;;
;;; X window system customization
;;;
(setq x-select-enable-clipboard t)

(when (display-graphic-p)
  (when (locate-library "fontutil")
    (require 'fontutil)
    (fontutil/set-font "scodepro-15")))
