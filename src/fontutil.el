;; -*-emacs-lisp-*-

;;;
;;; Font-related configuration
;;;

;; Note that the default font for Emacs should not be handled here.
;;

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


(when (display-graphic-p)
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


(defun fontutil/remove-unavailable (specs)
  "Remove unavailable font specification from the SPECS.

See `fontutil/fontconfig' for the definition of SPECS."
  (remq nil (mapcar (lambda (elt)
                      (let ((fc (car (cdr elt))))
                        (unless (listp fc)
                          (setq fc (list fc)))
                        (if (find-font (apply 'font-spec (car fc)))
                            elt)))
                    specs)))

(defvar fontutil/fontconfig
  (fontutil/remove-unavailable
   '(("scodepro-14" . (((:family "SourceCodePro" :size 14)
                        (hangul :family "NanumGothicCoding" :size 16)
                        (symbol :family "Symbola" :size 17))))
     ("scodepro-15" . (((:family "SourceCodePro" :size 15)
                        (hangul :family "NanumGothicCoding" :size 18)
                        (symbol :family "Symbola" :size 20))))
     ("scodepro-16" . (((:family "SourceCodePro" :size 16)
                        (hangul :family "NanumGothicCoding" :size 20)
                        (symbol :family "Symbola" :size 22))))
     ("scodepro-18" . (((:family "SourceCodePro" :size 18)
                        (hangul :family "NanumGothicCoding" :size 22)
                        (symbol :family "Symbola" :size 24))))
     ("scodepro-14" . (((:family "Source_Code_Pro" :size 14)
                        (hangul :family "NanumGothicCoding" :size 16)
                        (symbol :family "Symbola" :size 15))))
     ("scodepro-15" . (((:family "Source_Code_Pro" :size 15)
                        (hangul :family "NanumGothicCoding" :size 18)
                        (symbol :family "Symbola" :size 15))))
     ("scodepro-16" . (((:family "Source_Code_Pro" :size 16)
                        (hangul :family "NanumGothicCoding" :size 20)
                        (symbol :family "Symbola" :size 15))))
     ("scodepro-18" . (((:family "Source_Code_Pro" :size 18)
                        (hangul :family "NanumGothicCoding" :size 22)
                        (symbol :family "Symbola" :size 24))))
     ("inconsolata-14" . (((:family "Inconsolata" :size 14)
                           (hangul :family "NanumGothicCoding" :size 14)
                           (symbol :family "Symbola" :size 15))
                          ((line-spacing . 2))))
     ("inconsolata-15" . (((:family "Inconsolata" :size 15)
                           (hangul :family "NanumGothicCoding" :size 16)
                           (symbol :family "Symbola" :size 17))
                          ((line-spacing . 2))))
     ("inconsolata-16" . (((:family "Inconsolata" :size 16)
                           (hangul :family "NanumGothicCoding" :size 16)
                           (symbol :family "Symbola" :size 17))
                          ((line-spacing . 3))))
     ("inconsolata-20" . (((:family "Inconsolata" :size 20)
                           (hangul :family "NanumGothicCoding" :size 20)
                           (symbol :family "Symbola" :size 22))
                          ((line-spacing . 3))))
     ("monaco-14" . (((:family "Monaco" :size 14)
                      (hangul :family "NanumGothicCoding" :size 16)
                      (symbol :family "Symbola" :size 17))))
     ("monaco-15" . (((:family "Monaco" :size 15)
                      (hangul :family "NanumGothicCoding" :size 18)
                      (symbol :family "Symbola" :size 20))))
     ("monaco-16" . (((:family "Monaco" :size 16)
                      (hangul :family "NanumGothicCoding" :size 20)
                      (symbol :family "Symbola" :size 22))))))
  "Font and Frame configuration list

Each element has a form of (FONT-SPEC FRAME-SPEC), where
FONT-SPEC is a list of font properties, and FRAME-SPEC is a list
of frame parameters.

See `font-spec' for the list of font properties.

You can specify multiple font in FONT-SPEC; In this case, FONT-SPEC
is a list of font specs, where the first element is font properties
for the default font, and the rest elements are for the additional
font properties.  Except the first element, all elements have
TARGET as a prefix element.  See `set-fontset-font' for the possible
TARGET.  For example, if you want to use font XXX as the default font,
and you want to use YYY for Korean script, hangul, and use ZZZ for
symbolic glyphs, then the FONT-SPEC might be

  ((:family \"XXX\" :size 14)
   (hangul :family \"YYY\" :size 16)
   (symbol :family \"ZZZ\") :size 15)

As you can see in above, each font can have different size.  (This
is useful for CJK fonts)")

(defun fontutil/apply (name)
  "Apply font specification, NAME.

See also `fontutil/fontconfig' for the possible candidates"
  (let ((fonts (car (cdr (assoc name fontutil/fontconfig))))
        (params (cadr (cdr (assoc name fontutil/fontconfig)))))
    (unless (listp (car fonts))
      (setq fonts (list fonts)))
    (when (car fonts)
      (set-face-attribute 'default nil :font (apply 'font-spec (car fonts))))
    (dolist (aux (cdr fonts))
      (set-fontset-font t (car aux) (apply 'font-spec (cdr aux))))
    (when params
      (setq default-frame-alist params)
      (dolist (f (frame-list))
        (modify-frame-parameters f params)))))

(defun fontutil/set-font (name)
  "Apply font specification, NAME from `fontutil/fontconfig'"
  (interactive (list (completing-read "font: " fontutil/fontconfig)))
                                      ;; (lambda (f)
                                      ;;   (list-fonts (apply #'font-spec
                                      ;;                      (car (cdr f)))))
                                      ;; t nil)))
  (fontutil/apply name))

(provide 'fontutil)
