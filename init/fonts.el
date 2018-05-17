;; -*-emacs-lisp-*-

;;;
;;; Font-related configuration
;;;

;; Note that the default font for Emacs should not be handled here.
;;
;; For MacOSX (darwin), look at darwin.el.

(defun xftp (&optional frame)
  "Return t if FRAME support XFT font backend."
  (let ((xft-supported))
    (mapc (lambda (x) (if (eq x 'xft) (setq xft-supported t)))
          (frame-parameter frame 'font-backend))
    xft-supported))

(defun scale-default-font-height (factor &optional frame)
  "Scale the height of the default face
New height will be calculated by (* FACTOR old-face-height)"
  (let ((height (face-attribute 'default :height)))
    (set-face-attribute 'default frame :height (round (* height factor)))))


(when (or (display-graphic-p) (daemonp)) ; I like to enable this
                                         ; section even Aquamacs
                                         ; starts as a daemon

  ;; When Emacs uses graphic display,"control + mouse wheel up"
  ;; increases the default font size whereas "control + mouse wheel
  ;; down " decreases the size.

  ;; Note that if you call `mwheel-install' after this configuration,
  ;; both [C-mouse-4] and [C-mouse-5] bindings are cleared.
  ;;
  ;; It seems that mwheel package is automatically loaded in Emacs 22
  ;; or higher.  Thus, I do not need to call `mwheel-install' any longer.

  ;; In darwin, the wheel-up and wheel-down events are [C-wheel-up]
  ;; and [C-wheel-down] respectively.
  (let ((incr [C-mouse-4]) (decr [C-mouse-5]))
    (and (eq system-type 'darwin)
         (setq incr [C-wheel-up]
               decr [C-wheel-down]))

    (global-set-key incr (lambda ()
                           (interactive)
                           (scale-default-font-height 1.1
                                                      (selected-frame))
                           (message "New face height: %d"
                                    (face-attribute 'default :height))))
    (global-set-key decr (lambda ()
                           (interactive)
                           (scale-default-font-height 0.9
                                                      (selected-frame))
                           (message "New face height: %d"
                                    (face-attribute 'default :height))))))
