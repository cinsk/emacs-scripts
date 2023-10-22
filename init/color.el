;; -*-emacs-lisp-*-

;;;
;;; Provides simple/consistent interface to color-theme or native Emacs 24 theme.
;;;
;;; You can call either `themes/select-random' or `themes/select-next'.
;;;
;;; `themes/select-random' will apply theme from one of your
;;; favorites.  and `themes/select-next' will apply next theme among
;;; known themes.
;;;
;;; Note that the prefix argument to these function behaves
;;; differently.  With a prefix argument, `themes/select-random' will
;;; select a theme from all known themes.  On the contrary,
;;; `themes/select-next' will select a theme from favorite theme list
;;; if a prefix argument is given.
;;;
;;; This inconsistency is intentional; Normally, when I want to select
;;; a random theme, I only want to select a theme among my favorites.
;;; But, If I want to try all themes, I want to select a next theme
;;; from the whole themes.

(defun themes/safe-backend (&optional preferred)
  "Check if the theme backend, PREFERRED is available, and return the alternative"
  (or preferred (setq preferred 'theme))
  (cond ((eq preferred 'theme)
         (if (and (>= emacs-major-version 24)
                  (fboundp 'load-theme))
             'theme
           (if (locate-library "color-theme")
               (progn (require 'color-theme)
                      'color-theme))))
        ((eq preferred 'color-theme)
         (if (locate-library "color-theme")
             (progn (require 'color-theme)
                    'color-theme)
           (if (and (>= emacs-major-version 24)
                  (fboundp 'load-theme))
             'theme)))))

(defvar themes/backend (themes/safe-backend 'theme)
  "Backend selection. 'color-theme or 'theme")

(defvar themes/favorite-color-themes '(color-theme-deep-blue
                                       color-theme-cinsk-wood
                                       color-theme-charcoal-black
                                       color-theme-clarity
                                       color-theme-comidia
                                       color-theme-dark-blue2
                                       color-theme-dark-laptop
                                       ;; color-theme-taylor
                                       color-theme-billw
                                       color-theme-robin-hood)
  "My favorite color themes")

(defvar themes/favorite-themes '(deeper-blue
                                 manoj-dark
                                 tsdh-dark
                                 wheatgrass
                                 wombat

                                 leuven
                                 light-blue
                                 misterioso
                                 tsdh-light)
  "My favorite themes")

(defvar themes/last-theme nil
  "Last applied theme")

;;;
;;; color-theme settings
;;;
(setq color-theme-history-max-length 32)

;; If non-nil, new color theme will undo all settings made by previous
;; theme.  Normally, this is a bad idea, since some color themes do
;; not provide all face attributes.  However, if you want to find your
;; favorite theme using `themes/select-random' or
;; `color-theme-apply', setting this variable to t might help.
;;
;; (setq color-theme-is-cumulative nil)

(defun themes/apply (theme &optional cumulative interactive?)
  (when themes/backend
    (cond ((eq themes/backend 'color-theme)
           (let ((color-theme-is-cumulative cumulative))
             (funcall theme)))
          ((eq themes/backend 'theme)
           (unless cumulative
             (mapc 'disable-theme custom-enabled-themes))
           (load-theme theme)))
    (when interactive?
      (message "Theme %s installed" (symbol-name theme)))
    (setq themes/last-theme theme)))


(defun themes/select-random (&optional global)
  "Select random color? theme.

If optional FAVORITE-ONLY is non-nil, select color theme from
only in the `themes/favorite-color-themes' or `theme/favorite-themes'
depending on the theme backend.

This function returns the name of the color theme in string."
  (interactive "P")

  (let* ((thset (cond ((eq themes/backend 'color-theme)
                       (if global
                           color-themes
                         themes/favorite-color-themes))
                      ((eq themes/backend 'theme)
                       (if global
                         (custom-available-themes)
                         themes/favorite-themes))))
         (theme (nth (random (length thset)) thset)))
    (themes/apply theme nil (called-interactively-p 'any))))


(defun themes/next-theme-symbol (&optional global)
  "Return the next theme name."
  (when themes/backend
    (let* ((theme-set (if (eq themes/backend 'theme)
                          (if global
                              (custom-available-themes)
                            themes/favorite-themes)
                        (if global
                            color-themes
                          themes/favorite-color-themes)))
           (found (cl-member-if (lambda (elt)
                                  (if (consp elt)
                                      (eq themes/last-theme (car elt))
                                    (eq themes/last-theme elt)))
                                theme-set)))
      (setq found (cadr found))
      (let ((ret (or found (car theme-set))))
        (if (consp ret)
            (car ret)
          ret)))))

(defun themes/select-next (&optional favorite-only)
  (interactive "P")
  (let ((theme (themes/next-theme-symbol (not favorite-only))))
    (themes/apply theme nil (called-interactively-p 'any))))


;; (defun color-themes-next-symbol (theme)
;;   "Return the next color-theme symbol of THEME"
;;   (let ((found 0) (next nil))
;;     (catch 'found
;;       (mapc (lambda (entry)
;;               (if (and (= found 1) (null next))
;;                   (progn (setq next (car entry))
;;                          (throw 'found t)))
;;               (if (eq (car entry) theme)
;;                   (setq found 1)))
;;             color-themes))
;;     (if (and (= found 1) (null next))
;;         (setq next (car (caddr color-themes)))
;;       next)))

(defun color-rgb-256 (color &optional frame)
  (and (stringp color)
       (setq color (color-values color frame)))
  (let ((maxval (car (color-values "#ffffff"))))
    (mapcar (lambda (x) (/ (* (float x) 255) maxval)) color)))

(defun color-rgb-system (color256 &optional frame)
  (let ((maxval (car (color-values "#ffffff"))))
    (if (stringp color256)
        (color-values color256 frame)
      (mapcar (lambda (x) (/ (* (float x) maxval) 255)) color256))))

(defun color-string (color &optional frame)
  (if (stringp color)
      color
      (let* ((maxval (car (color-values "#ffffff")))
             (norm (mapcar (lambda (x) (/ (* x 255) maxval)) color)))
        (format "#%02x%02x%02x" (car norm) (cadr norm) (caddr norm)))))

(defun color-brightness (color)
  "Return the brightness (in YIQ color space) of COLOR, range [0, 256].

COLOR should be a list of three integers, returned from `color-values'."
  ;; http://www.w3.org/WAI/ER/WD-AERT/#color-contrast
  (let* ((maxval (car (color-values "#ffffff")))
         (cvs (mapcar (lambda (x) (/ (* x 255) (float maxval))) color))
         (red (car cvs))
         (green (cadr cvs))
         (blue (caddr cvs)))
    (round (/ (+ (* red 299) (* green 587) (* blue 114)) 1000.0))))

(defun dark-theme-p (&optional frame)
  "Return t if the background of the FRAME is dark color"
  (< (color-brightness (face-background 'default nil 'default)) 125))


;; I modified `list-colors-print' so that it will use black/white foreground
;; depending on the brightness of the background color.  The origin was from
;; Emacs 24.4.1, facemenu.el
(defun list-colors-print (list &optional callback)
  (let ((callback-fn
         (if callback
             `(lambda (button)
                (funcall ,callback (button-get button 'color-name))))))
    (dolist (color list)
      (if (consp color)
          (if (cdr color)
              (setq color (sort color (lambda (a b)
                                        (string< (downcase a)
                                                 (downcase b))))))
        (setq color (list color)))
      (let* ((opoint (point))
             (color-values (color-values (car color)))
             (light-p (> (color-brightness color-values) 125))
             ;; (light-p (>= (apply 'max color-values)
             ;;           (* (car (color-values "white")) .5)))
             )


        (insert (car color))
        (indent-to 22)
        (put-text-property opoint (point) 'face `(:background ,(car color)
                                                  :foreground ,(if light-p "black" "white")))
        (put-text-property
         (prog1 (point)
           (insert " ")
           ;; Insert all color names.
           (insert (mapconcat 'identity color ",")))
         (point)
         'face (list :foreground (car color)))
        (insert (propertize " " 'display '(space :align-to (- right 9))))
        (insert " ")
        (insert (propertize
                 (apply 'format "#%02x%02x%02x"
                        (mapcar (lambda (c) (lsh c -8))
                                color-values))
                 'mouse-face 'highlight
                 'help-echo
                 (let ((hsv (apply 'color-rgb-to-hsv
                                   (color-name-to-rgb (car color)))))
                   (format "H:%.2f S:%.2f V:%.2f"
                           (nth 0 hsv) (nth 1 hsv) (nth 2 hsv)))))
        (when callback
          (make-text-button
           opoint (point)
           'follow-link t
           'mouse-face (list :background (car color)
                             :foreground (if light-p "black" "white"))
           'color-name (car color)
           'action callback-fn)))
      (insert "\n"))
    (goto-char (point-min))))


(defun color-theme-apply (&optional arg)
  "Apply the color theme.

If the argument is :random, this applies any color theme randomly,
or if the argument is :next, this applies the next color theme in the
installed color theme list.  or if the argument is a symbol indicates
the color-theme function, it applies that color theme."
  (cond ((fboundp arg)  (apply arg nil))
        ((eq arg :random)  (themes/select-random))
        ((eq arg :next) (let ((theme (themes/next-theme-symbol)))
                          (apply theme nil)
                          (message "%s installed" (symbol-name theme))))
        (t (error "Wrong type of argument"))))

;; Unlike prior version of Emacs, Emacs 24 does not provide
;; `plist-to-alist' which color-theme 6.x uses.
(unless (fboundp 'plist-to-alist)
  (defun plist-to-alist (the-plist)
    (defun get-tuple-from-plist (the-plist)
      (when the-plist
        (cons (car the-plist) (cadr the-plist))))

    (let ((alist '()))
      (while the-plist
        (add-to-list 'alist (get-tuple-from-plist the-plist))
        (setq the-plist (cddr the-plist)))
      alist)))

(when window-system

  (when (locate-library "color-theme")
    (require 'color-theme)
    (and (fboundp 'color-theme-initialize)
         (color-theme-initialize))

    (and (locate-library "pink-bliss")
         (require 'pink-bliss))

    (and (locate-library "cinsk-wood")
         (require 'cinsk-wood)))

  (global-set-key [(control f1)] 'themes/select-random)
  (global-set-key [(control f2)] 'themes/select-next)

  ;; If you want to select random color theme on every new frame,
  ;; uncomment this.
  ;; (add-hook 'after-make-frame-functions 'set-frame-color-theme)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (if (and (boundp 'color-theme-is-global)
                       (not color-theme-is-global))
                  (themes/select-random 'favorite))))

  ;; It seems that frame-local color-theme is somewhat unstable in
  ;; recent version of Emacs.
  (setq color-theme-is-global t)

  (random t)

  ;; Select random color theme from my favorite list
  ;;(themes/select-random)
  )
