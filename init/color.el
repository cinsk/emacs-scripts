;; -*-emacs-lisp-*-

;;;
;;; color-theme settings
;;;
(setq color-theme-history-max-length 32)

;; If non-nil, new color theme will undo all settings made by previous
;; theme.  Normally, this is a bad idea, since some color themes do
;; not provide all face attributes.  However, if you want to find your
;; favorite theme using `color-theme-apply-random' or
;; `color-theme-apply', setting this variable to t might help.
;;
;; (setq color-theme-is-cumulative nil)

(defvar color-theme-favorites '(color-theme-deep-blue
                                color-theme-cinsk-wood
                                color-theme-charcoal-black
                                color-theme-clarity
                                color-theme-comidia
                                color-theme-dark-blue2
                                color-theme-dark-laptop
                                ;; color-theme-taylor
                                color-theme-billw
                                color-theme-robin-hood)
  "My favorite color theme list")

(defun color-theme-select-favorite (&optional arg)
  "Apply one color theme from `color-theme-favorites'.

If called with prefix arguments, it will undo all settings made
by previous color theme.  Otherwise the new theme is installed on
top of each other."
  (interactive "P")

  (let ((color-theme-is-cumulative (if arg nil color-theme-is-cumulative)))
    (let ((theme (color-theme-apply-random 'favorite-only)))
      (message "%s installed" theme))))

(defun color-theme-apply-random (&optional favorite-only frame)
  "Select random color theme.

If optional FAVORITE-ONLY is non-nil, select color theme from
only in the `color-theme-favorites'.  The color theme is applied
to FRAME (nil for current frame).

This function returns the name of the color theme in string."
  (let* ((theme-list (if favorite-only
                         color-theme-favorites
                       color-themes))
         (selected (nth (random (length theme-list)) theme-list))
         (theme-func (if (consp selected) (car selected) selected))
         (theme-name (if (consp selected)
                         (cadr selected)
                       (symbol-name theme-func))))
    (with-selected-frame (or frame (selected-frame))
      (funcall theme-func)
      theme-name)))


(defun color-themes-next-symbol (theme)
  "Return the next color-theme symbol of THEME"
  (let ((found 0) (next nil))
    (catch 'found
      (mapc (lambda (entry)
              (if (and (= found 1) (null next))
                  (progn (setq next (car entry))
                         (throw 'found t)))
              (if (eq (car entry) theme)
                  (setq found 1)))
            color-themes))
    (if (and (= found 1) (null next))
        (setq next (car (caddr color-themes)))
      next)))


(defun color-theme-apply (&optional arg)
  "Apply the color theme.

If the argument is :random, this applies any color theme randomly,
or if the argument is :next, this applies the next color theme in the
installed color theme list.  or if the argument is a symbol indicates
the color-theme function, it applies that color theme."
  (cond ((fboundp arg)  (apply arg nil))
        ((eq arg :random)  (color-theme-apply-random))
        ((eq arg :next) (let ((theme (color-theme-next-symbol)))
                          (apply theme nil)
                          (message "%s installed" (symbol-name theme))))
        (t (error "Wrong type of argument"))))

(defun color-theme-next-symbol ()
  "Return the next color-theme symbol of the last applied color theme.

This function works iff color-theme-history-max-length is not NIL"
  (if (null color-theme-history)
      (car (car color-themes))
    (color-themes-next-symbol (car (car color-theme-history)))))


(when (and window-system
           (locate-library "color-theme"))
  (require 'color-theme)
  (and (fboundp 'color-theme-initialize)
       (color-theme-initialize))

  (and (locate-library "pink-bliss")
       (require 'pink-bliss))

  (and (locate-library "cinsk-wood")
       (require 'cinsk-wood))

  (global-set-key [(control f1)] 'color-theme-select-favorite)
  (global-set-key [(control f2)] '(lambda ()
                                    (interactive)
                                    (color-theme-apply :next)))

  ;; If you want to select random color theme on every new frame,
  ;; uncomment this.
  ;; (add-hook 'after-make-frame-functions 'set-frame-color-theme)
  (add-hook 'after-make-frame-functions
            (lambda (frame) (color-theme-apply-random 'favorite frame)))
  ;; color-theme-* is frame-local from now.
  (setq color-theme-is-global nil)

  (random t)
  ;; Select random color theme from my favorite list
  (color-theme-select-favorite)
)
