;; -*-emacs-lisp-*-

;;;
;;; Font-related configuration
;;;

;; Note that the default font for Emacs should not be handled here.
;;

(defvar fontutil/set-font-hook '(fontutil/set-fixed-pitch-serif)
  "list of functions that will be called on `fontutil/set-font'.")

(defvar fontutil/must-set-fixed-pitch-serif 'undefined
  "If non-nil, `fontutil/set-fixed-pitch-serif' will proceed.")


(defun fontutil/xftp (&optional frame)
  "Return t if FRAME support XFT font backend."
  (let ((xft-supported))
    (mapc (lambda (x) (if (eq x 'xft) (setq xft-supported t)))
          (frame-parameter frame 'font-backend))
    xft-supported))

(defun fontutil/scale-default-height (factor &optional frame)
  "Scale the height of the default face
New height will be calculated by (* FACTOR old-face-height)"
  (let ((height (face-attribute 'default :height)))
    (set-face-attribute 'default frame :height (round (* height factor)))))


(defun fontutil/increase-default-height ()
  (interactive)
  (fontutil/scale-default-height 1.1
                             (selected-frame))
  (message "New face height: %d"
           (face-attribute 'default :height)))

(defun fontutil/decrease-default-height ()
  (interactive)
  (fontutil/scale-default-height 0.9
                             (selected-frame))
  (message "New face height: %d"
           (face-attribute 'default :height)))

(defun fontutil/install-mouse-wheel ()
  (when (or (display-graphic-p) (daemonp))
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

      (global-set-key incr 'fontutil/increase-default-height)
      (global-set-key decr 'fontutil/decrease-default-height))))

;;;###autoload
(defun fontutil/reload ()
  (interactive)
  (setq fontutil/fontconfig (fontutil/remove-unavailable
                             fontutil/default-fontconfig)))

(defun fontutil/remove-unavailable (specs &optional verbose)
  "Remove unavailable font specification from the SPECS.

If optional VERBOSE is non-nil, it prints messages if the font
not found.  See `fontutil/fontconfig' for the definition of
SPECS."
  (mapcan (lambda (elt)
            (let ((fc (car (cdr elt))))
              (unless (listp fc)
                (setq fc (list fc)))
              (if (find-font (apply 'font-spec (car fc)))
                  (progn
                    (when verbose
                      (dolist (s (cdr fc))
                        (if (not (find-font (apply 'font-spec (cdr s))))
                            (message "warning: font-spec %S not found" s))))
                    (list elt))
                (when verbose
                  (message "error: font-spec %S not found" (car fc))
                  nil))))
          specs))

;;
;; Debugging:
;; - `list-faces-display' can help to browse all faces.
;; - (dolist (i (list-fonts (font-spec :size 14))) (print i))
;;
;;
(defvar fontutil/default-fontconfig
  '((scodepro-14 . (((:family "SourceCodePro" :size 14 :weight normal)
                     (hangul :family "NanumGothicCoding" :size 16)
                     (symbol :family "Symbola" :size 17))))
    (scodepro-15 . (((:family "SourceCodePro" :size 15 :weight normal)
                     (hangul :family "NanumGothicCoding" :size 18)
                     (symbol :family "Symbola" :size 20))))
    (scodepro-16 . (((:family "SourceCodePro" :size 16 :weight normal)
                     (hangul :family "NanumGothicCoding" :size 20)
                     (symbol :family "Symbola" :size 22))))
    (scodepro-18 . (((:family "SourceCodePro" :size 18 :weight normal)
                     (hangul :family "NanumGothicCoding" :size 22)
                     (symbol :family "Symbola" :size 24))))
    (scodepro-20 . (((:family "SourceCodePro" :size 20 :weight normal)
                     (hangul :family "NanumGothicCoding" :size 22)
                     (symbol :family "Symbola" :size 24))))
    (scodepro-14 . (((:family "Source_Code_Pro" :size 14)
                     (hangul :family "NanumGothicCoding" :size 16)
                     (symbol :family "Symbola" :size 15))))
    (scodepro-15 . (((:family "Source_Code_Pro" :size 15)
                     (hangul :family "NanumGothicCoding" :size 18)
                     (symbol :family "Symbola" :size 15))))
    (scodepro-16 . (((:family "Source_Code_Pro" :size 16)
                     (hangul :family "NanumGothicCoding" :size 20)
                     (symbol :family "Symbola" :size 15))))
    (scodepro-18 . (((:family "Source_Code_Pro" :size 18)
                     (hangul :family "NanumGothicCoding" :size 22)
                     (symbol :family "Symbola" :size 24))))
    (scodepro-14 . (((:family "Source Code Pro" :size 14)
                     (hangul :family "NanumGothicCoding" :size 16)
                     (symbol :family "Symbola" :size 15))
                    ((line-spacing . 1))))
    (scodepro-15 . (((:family "Source Code Pro" :size 15)
                     (hangul :family "NanumGothicCoding" :size 18)
                     (symbol :family "Symbola" :size 15))))
    (scodepro-16 . (((:family "Source Code Pro" :size 16)
                     (hangul :family "NanumGothicCoding" :size 20)
                     (symbol :family "Symbola" :size 15))))
    (scodepro-18 . (((:family "Source Code Pro" :size 18)
                     (hangul :family "NanumGothicCoding" :size 22)
                     (symbol :family "Symbola" :size 24))))
    (ubuntu-16 . (((:family "UbuntuMono" :size 16)
                   (hangul :family "NanumGothicCoding" :size 16)
                   (symbol :family "Symbola" :size 17))
                  ((line-spacing . 4))))
    (ubuntu-18 . (((:family "UbuntuMono" :size 18)
                   (hangul :family "NanumGothicCoding" :size 18)
                   (symbol :family "Symbola" :size 20))
                  ((line-spacing . 4))))

    (ubuntu-20 . (((:family "UbuntuMono" :size 20)
                   (hangul :family "NanumGothicCoding" :size 20)
                   (symbol :family "Symbola" :size 22))
                  ((line-spacing . 4))))

    (ubuntu-22 . (((:family "UbuntuMono" :size 22)
                   (hangul :family "NanumGothicCoding" :size 22)
                   (symbol :family "Symbola" :size 22))
                  ((line-spacing . 4))))

    (dejavu-16 . (((:family "DejaVuSansMono" :size 16)
                   (hangul :family "NanumGothicCoding" :size 16)
                   (symbol :family "Symbola" :size 17))
                  ((line-spacing . 4))))
    (dejavu-18 . (((:family "DejaVuSansMono" :size 18)
                   (hangul :family "NanumGothicCoding" :size 18)
                   (symbol :family "Symbola" :size 20))
                  ((line-spacing . 4))))

    (dejavu-20 . (((:family "DejaVuSansMono" :size 20)
                   (hangul :family "NanumGothicCoding" :size 20)
                   (symbol :family "Symbola" :size 22))
                  ((line-spacing . 4))))

    (nova-16 . (((:family "NovaMono" :size 16)
                 (hangul :family "NanumGothicCoding" :size 18)
                 (symbol :family "Symbola" :size 20))))
    (nova-18 . (((:family "NovaMono" :size 18)
                 (hangul :family "NanumGothicCoding" :size 20)
                 (symbol :family "Symbola" :size 22))))

    (inconsolata-14 . (((:family "Inconsolata" :size 14)
                        (hangul :family "NanumGothicCoding" :size 14)
                        (symbol :family "Symbola" :size 15))
                       ((line-spacing . 2))))
    (inconsolata-15 . (((:family "Inconsolata" :size 15)
                        (hangul :family "NanumGothicCoding" :size 16)
                        (symbol :family "Symbola" :size 17))
                       ((line-spacing . 2))))
    (inconsolata-16 . (((:family "Inconsolata" :size 16)
                        (hangul :family "NanumGothicCoding" :size 16)
                        (symbol :family "Symbola" :size 17))
                       ((line-spacing . 3))))
    (inconsolata-20 . (((:family "Inconsolata" :size 20)
                        (hangul :family "NanumGothicCoding" :size 20)
                        (symbol :family "Symbola" :size 22))
                       ((line-spacing . 3))))

    (consolas-14 . (((:family "Consolas" :size 14)
                     (hangul :family "NanumGothicCoding" :size 16)
                     (symbol :family "Symbola" :size 16))
                    ((line-spacing . 2))))
    (consolas-15 . (((:family "Consolas" :size 15)
                     (hangul :family "NanumGothicCoding" :size 16)
                     (symbol :family "Symbola" :size 16))
                    ((line-spacing . 2))))
    (consolas-16 . (((:family "Consolas" :size 16)
                     (hangul :family "NanumGothicCoding" :size 18)
                     (symbol :family "Symbola" :size 17))
                    ((line-spacing . 3))))
    (consolas-20 . (((:family "Consolas" :size 20)
                     (hangul :family "NanumGothicCoding" :size 22)
                     (symbol :family "Symbola" :size 25))
                    ((line-spacing . 3))))

    (monaco-14 . (((:family "Monaco" :size 14)
                   (hangul :family "NanumGothicCoding" :size 16)
                   (symbol :family "Symbola" :size 17))))
    (monaco-15 . (((:family "Monaco" :size 15)
                   (hangul :family "NanumGothicCoding" :size 18)
                   (symbol :family "Symbola" :size 20))))
    (monaco-16 . (((:family "Monaco" :size 16)
                   (hangul :family "NanumGothicCoding" :size 22)
                   (symbol :family "Symbola" :size 22))))

    (menlo-14 . (((:family "Menlo" :size 14)
                  (hangul :family "NanumGothicCoding" :size 16)
                  (symbol :family "Symbola" :size 20))))
    (menlo-16 . (((:family "Menlo" :size 16)
                  (hangul :family "NanumGothicCoding" :size 20)
                  (symbol :family "Symbola" :size 20))))
    (menlo-18 . (((:family "Menlo" :size 18)
                  (hangul :family "NanumGothicCoding" :size 22)
                  (symbol :family "Symbola" :size 20))))

    (pt-14 . (((:family "PT Mono" :size 14)
               (hangul :family "NanumGothicCoding" :size 16)
               (symbol :family "Symbola" :size 20))
              ((line-spacing . 4))))
    (pt-16 . (((:family "PT Mono" :size 16)
               (hangul :family "NanumGothicCoding" :size 20)
               (symbol :family "Symbola" :size 20))))
    (pt-18 . (((:family "PT Mono" :size 18)
               (hangul :family "NanumGothicCoding" :size 22)
               (symbol :family "Symbola" :size 20))
              ((line-spacing . 3))))

    (hack-14 . (((:family "Hack" :size 14)
                 (hangul :family "NanumGothicCoding" :size 16)
                 (symbol :family "Symbola" :size 20))))
    (hack-16 . (((:family "Hack" :size 16)
                 (hangul :family "NanumGothicCoding" :size 20)
                 (symbol :family "Symbola" :size 20))))
    (hack-18 . (((:family "Hack" :size 18)
                 (hangul :family "NanumGothicCoding" :size 22)
                 (symbol :family "Symbola" :size 20))))

    (noto-14 . (((:family "Noto Sans Mono" :size 14)
                 (hangul :family "Noto Sans Mono CJK KR" :size 17)
                 (symbol :family "Noto Sans Symbols" :size 16))))
    (noto-16 . (((:family "Noto Sans Mono" :size 16)
                 (hangul :family "Noto Sans Mono CJK KR" :size 22)
                 (symbol :family "Noto Sans Symbols" :size 16))))
    (noto-18 . (((:family "Noto Sans Mono" :size 18)
                 (hangul :family "Noto Sans Mono CJK KR" :size 24)
                 (symbol :family "Noto Sans Symbols" :size 18))))

    (firacode-14 . (((:family "Fira Code" :size 14)
                     (hangul :family "NanumGothicCoding" :size 16)
                     (symbol :family "Symbola" :size 20))))
    (firacode-15 . (((:family "Fira Code" :size 15)
                     (hangul :family "NanumGothicCoding" :size 18)
                     (symbol :family "Symbola" :size 20))))
    (firacode-16 . (((:family "Fira Code" :size 16)
                     (hangul :family "NanumGothicCoding" :size 20)
                     (symbol :family "Symbola" :size 15))))
    (firacode-18 . (((:family "Fira Code" :size 18)
                     (hangul :family "NanumGothicCoding" :size 22)
                     (symbol :family "Symbola" :size 24))))
    )
  "Font and Frame configuration list

Each element has a form of (FONT-SPECS FRAME-SPEC), where
FONT-SPECS is a list of font properties, and FRAME-SPEC is a list
of frame parameters.

See `font-spec' for the list of font properties.

You can specify multiple font in FONT-SPECS; In this case,
FONT-SPECS is a list of font specs, where the first element is
font properties for the default font, and the rest elements are
for the additional font properties (usually for a fontset).
Except the first element, all elements have TARGET as a prefix
element.  See `set-fontset-font' for the possible TARGET.  For
example, if you want to use font XXX as the default font, and you
want to use YYY for Korean script, hangul, and use ZZZ for
symbolic glyphs, then the FONT-SPEC might be

  ((:family \"XXX\" :size 14)
   (hangul :family \"YYY\" :size 16)
   (symbol :family \"ZZZ\") :size 15)

Here, 'hangul and 'symbol are the name of the script.  See
`script-representative-chars' for the list of known scripts.

As you can see in above, each font can have different size.  (This
is useful for CJK fonts)")

(defvar fontutil/fontconfig
  (fontutil/remove-unavailable fontutil/default-fontconfig)
  "Unavailable font config -- See also `fontutil/default-fontconfig'")

(defun fontutil/apply (name)
  "Apply font specification, NAME.

See also `fontutil/fontconfig' for the possible candidates"
  (let ((fonts (car (alist-get name fontutil/fontconfig)))
        (params (cadr (alist-get name fontutil/fontconfig))))
    ;;
    ;;
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

;;;###autoload
(defun fontutil/set-font (name)
  "Apply font specification, NAME from `fontutil/fontconfig'"
  (interactive (list (completing-read "font: " fontutil/fontconfig)))
                                      ;; (lambda (f)
                                      ;;   (list-fonts (apply #'font-spec
                                      ;;                      (car (cdr f)))))
                                      ;; t nil)))
  (setq name (intern name))
  (fontutil/apply name)
  (run-hook-with-args 'fontutil/set-font-hook name))


(defun fontutil/set-fixed-pitch-serif (name)
  ;; I found that on certain cases, that face fixed-pitch-serif does
  ;; not have proper font attached, so that info manual looks very
  ;; awkward.  This function will take the font family of the
  ;; fontconfig, and set it forcefully.
  (let ((fonts (car (alist-get name fontutil/fontconfig))))
    (unless (listp (car fonts))
      (setq fonts (list fonts)))
    (let ((fnt (car fonts)))
      (when (eq fontutil/must-set-fixed-pitch-serif 'undefined)
        (let ((old (find-font (font-spec :family
                                         (face-attribute 'fixed-pitch-serif
                                                         :family)))))
          (setq fontutil/must-set-fixed-pitch-serif (not old))))

      (when (eq fontutil/must-set-fixed-pitch-serif t)
        (let ((family (plist-get fnt :family)))
          (when family
            (set-face-attribute 'fixed-pitch-serif nil :family family)))))))


(provide 'fontutil)
