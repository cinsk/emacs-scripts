;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;
;;; Window and Frame related configuration
;;;

;; `new-frame' was deprecated in favor of `make-frame'.  I still want
;; to use it (interactively).
(defalias 'new-frame #'make-frame)

;; Set the default value for the title bar of the Emacs frame.
;;
;; The possible format specifiers (e.g. %F or %b) are explained in
;; the documentation of `mode-line-format'.

(setq frame-title-format
      (if (= (user-uid) 0)
          ;; If Emacs running as root, print "ROOT" just in case
          "%F - ROOT - %b"
        "%F - %b"))

(setq icon-title-format
      (if (= (user-uid) 0)
          "%F - ROOT"
        "%F"))


(when (locate-library "wfutils")
  (require 'wfutils)
  (my/move-key :from "C-x f" :to "C-x F")
  (wfu-mode 1))



(defun toggle-current-window-dedication ()
  "Toggle current window as dedicated"
  (interactive)
  ;; TODO: if the window is dedicated by other means than this
  ;; function, we should not revert the dedicated state.
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))
(keymap-global-set "<Scroll_Lock>" #'toggle-current-window-dedication)

(defun current-frame-configuration-only (&optional frame)
  "Return a list describing the positions and states of FRAME only.

This function behaves similar to `current-frame-configuration'
except the return value contains the information of specified
FRAME only."
  (if (null frame)
      (setq frame (window-frame (selected-window))))
  (let ((fc nil))
    (mapc (lambda (f)
            (if (eq (car f) frame)
                (setq fc f)))
          (cdr (current-frame-configuration)))
    (list 'frame-configuration fc)))


(defun set-this-frame-configuration (configuration)
  "Restore the frame to the state described by CONFIGURATION.

This function behaves similar to `set-frame-configuration' except
it will not affect the other frames that are not described in
CONFIGURATION."
  (let ((old-func (symbol-function 'iconify-frame)))
    (fset 'iconify-frame (lambda (&optional frame) nil))
    (set-frame-configuration configuration t)
    (fset 'iconify-frame old-func)
    nil))


(when (require 'tab-bar nil t)
  (when (boundp 'tab-prefix-map)
    (keymap-set tab-prefix-map "[" #'tab-previous)
    (keymap-set tab-prefix-map "]" #'tab-next))
  (keymap-global-set "C-M-{" #'tab-previous)
  (keymap-global-set "C-M-}" #'tab-next))

(defun run-command-other-frame (command)
  "Run COMMAND in a new frame."
  (interactive "CC-x 5 M-x ")
  (select-frame (make-frame))
  (call-interactively command))
(keymap-global-set "C-x 5 M-x" #'run-command-other-frame)


;; (defun frame-position-for-resizing (width height &optional frame display)
;;   "Return the good frame position (LEFT TOP WIDTH HEIGHT) to satisfy the
;; new frame size WIDTH and HEIGHT regarding to the current display"
;;   (or width (setq width (frame-width frame)))
;;   (or height (setq height (frame-height frame)))
;;   (let* ((left (frame-parameter frame 'left))
;;          (top  (frame-parameter frame 'top))
;;          (margin (frame-margin frame))
;;          ;; width and height of current frame in characters.
;;          (cur-width (frame-width frame))
;;          (cur-height (frame-height frame))
;;          ;; width and height of the display in pixels.
;;          (disp-width (display-pixel-width display))
;;          (disp-height (display-pixel-height display))
;;          ;; width and height of the character in pixels.
;;          (char-width (frame-char-width frame))
;;          (char-height (frame-char-height frame))
;;          ;; width and height of the new frame size in pixels.
;;          (pwidth (+ (* char-width width) (car margin)))
;;          (pheight (+ (* char-height height) (cdr margin))))
;;     (if (> pwidth disp-width)
;;         ;; WIDTH is too large for the display.
;;         (setq width (/ (- disp-width (car margin)) char-width)
;;               pwidth (+ (* width char-width) (car margin))
;;               ;; Since `frame-margin' cannot determine the exact
;;               ;; margin regarding to the window system, it's better to
;;               ;; set LEFT zero.
;;               left 0))
;;     (if (> pheight disp-height)
;;         ;; HEIGHT is too large for the display.
;;         (setq height (/ (- disp-height (cdr margin)) char-height)
;;               pheight (+ (* width char-height) (cdr margin))
;;               ;; Since `frame-margin' cannot determine the exact
;;               ;; margin regarding to the window system, it's better to
;;               ;; set TOP zero.
;;               top 0))
;;     (when (> (+ left pwidth) disp-width)
;;       ;; Current LEFT cannot satisfy WIDTH
;;       (setq left (- left (+ (* (- width cur-width) char-width)
;;                             (ceiling (/ (car margin) 2.0))))
;;             left (if (< left 0) 0 left)))
;;     (when (> (+ top pheight) disp-height)
;;       ;; Current LEFT cannot satisfy HEIGHT
;;       (setq top (- top (+ (* (- height cur-height) char-height)
;;                           (ceiling (/ (cdr margin) 2.0))))
;;             top (if (< top 0) 0 top)))
;;     (list left top width height)))

(defun frame-max-available-width (&optional frame)
  "Return the maximum value for the possible frame width regards
to the display width"
  (let ((width (frame-width frame))
        (char-width (frame-char-width frame))
        (pwidth (frame-pixel-width frame)))
    (- (/ (- (display-pixel-width) (- pwidth (* width char-width)))
          char-width)
       ;; For safety, subtract 2 from the max-width because we don't
       ;; know the exact margin.
       ;;
       ;; TODO: Using zero in MacOS X seems to be fine.  Check in other system.
       0)))


(defun load-or-save-frame-configuration (&optional arg)
  (interactive "P")
  (let* ((cmdvec (this-command-keys))
         (lastchar (aref cmdvec (1- (length cmdvec)))))
    (if current-prefix-arg
        (progn
          (frameset-to-register lastchar)
          (message "frame-configuration saved to register %c" lastchar))
      (jump-to-register lastchar)
      (message "frame-configuration restored from register %c" lastchar))))


(defvar frame-configuration-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "1" #'load-or-save-frame-configuration)
    (keymap-set map "2" #'load-or-save-frame-configuration)
    (keymap-set map "3" #'load-or-save-frame-configuration)
    (keymap-set map "4" #'load-or-save-frame-configuration)
    (keymap-set map "5" #'load-or-save-frame-configuration)
    (keymap-set map "6" #'load-or-save-frame-configuration)
    (keymap-set map "7" #'load-or-save-frame-configuration)
    (keymap-set map "8" #'load-or-save-frame-configuration)
    (keymap-set map "9" #'load-or-save-frame-configuration)
    (keymap-set map "0" #'load-or-save-frame-configuration)
    map)
  "hotkey keymap for save/load frameconfiguration")

(keymap-global-set "C-<f3>" frame-configuration-map)




;; To prevent vertical split
(setq split-width-threshold 200)
