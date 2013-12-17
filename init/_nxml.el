;; -*-emacs-lisp-*-

;;;
;;; XML configuration
;;;

(eval-when-compile
  (require 'nxml-mode nil t))

(when nil
  (when (and (locate-library "rng-auto")
             (locate-library "rng-auto"))
    ;; For legacy nxml-mode which does not use `provide' for nxml-mode.
    (load-library "rng-auto"))


  ;; For nxml-version less than or equal to "20041004" (my Gentoo), I
  ;; need to load rng-loc.el to use `rng-schema-locating-files'.
  (when (and (boundp 'nxml-version)
             (locate-library "rng-loc")
             (not (string-lessp "20041004" nxml-version)))
    (load (locate-library "rng-loc")))

  (when (locate-library "nxml-mode")
    ;; Emacs 24.x built-int nxml-mode provides a package to be used with
    ;; `require'.

    (require 'nxml-mode)

    ;; Ubuntu bundled nxml requires to load rng-nxml explicitly,
    ;; otherwise rng-schema-locating-files* variables will not be
    ;; defined.
    (when (locate-library "rng-nxml")
      (require 'rng-nxml)
      (and (fboundp 'rng-nxml-mode-init)
           (rng-nxml-mode-init)))))

;; `sgml-mode' adds an entry to `magic-mode-alist' so that
;; `auto-mode-alist' to `nxml-mode' might not work.  To work around
;; this, define an alias for `xml-mode' to `nxml-mode'.
;;
;; It may have no harm or no effect since recent version of Emacs
;; already alias `xml-mode' to `nxml-mode'.
(defalias 'xml-mode 'nxml-mode)

(defun lzx-nxml-mode ()
  "OpenLaszlo XML Mode"
  (interactive)
  (nxml-mode)
  (make-local-variable 'nxml-child-indent)
  (setq nxml-child-indent 4))

;;
;; python-mode version 6.0.10 has `(defvar nxml-child-indent nil)' in
;; its source code.  This causes indentation on nxml-mode not working.
;; So, I just define the variable `nxml-child-indent' forcefully to
;; remedy this.
(setq nxml-child-indent 2)

(eval-after-load "nxml-mode"
  '(progn
     ;; Make a slash automatically completes the end-tag
     (setq nxml-slash-auto-complete-flag t)
     (define-key nxml-mode-map [(control ?c) (control ?e)]
       'nxml-enclose-paragraph)

     ;; install abbrev table
     (add-hook 'nxml-mode-hook (function (lambda nil (abbrev-mode 1))))))


(when (fboundp 'nxml-mode)
  (setq auto-mode-alist (cons '("\\.lzx\\'" . lzx-nxml-mode)
                              auto-mode-alist))

  (setq auto-mode-alist (cons '("\\.\\(xml\\|pvm\\|rss\\)\\'" . nxml-mode)
                              auto-mode-alist)))

;; All .html files that I(cinsk) generate are XHTML files.
;; Thus, I choose `nxml-mode' over other major modes.
(add-to-list 'auto-mode-alist '("/public_html/.*\\.s?html?\\'" . nxml-mode))


(defvar cinsk/nxml-initialized nil
  "Non-nil if `cinsk/nxml-init' is called")

(defadvice nxml-mode (before cinsk/nxml-init
                             ())
  "package-wide init for `nxml-mode'."
  (unless cinsk/nxml-initialized
    (setq cinsk/nxml-initialized t)

    ;; Current nxml-mode package (nxml-mode-20041004-r3) in Gentoo Linux
    ;; install schema files (schemas.xml) in
    ;; "/usr/share/emacs/etc/nxml-mode/schema/", although
    ;; `rng-schema-locating-files-default' points to
    ;; "/usr/share/emacs/site-lisp/nxml-mode/schema/".
    ;;
    ;; If `rng-schema-locating-files-default' points wrong place, warn
    ;; for malfunction of nxml-mode's auto-completion (C-RET)
    (when (boundp 'rng-schema-locating-files-default)
      (dolist (file rng-schema-locating-files-default)
        (if (string-match "^/usr/" file)
            (if (not (file-readable-p file))
                (lwarn '(dot-emacs) :warning
                       "cannot access default schema for nxml-mode")))))

    ;;
    ;; html5 related setting
    ;;
    (let ((html5-el (concat (file-name-as-directory
                             (expand-file-name user-emacs-directory))
                            "html5-el"))
          (schema (concat (file-name-as-directory
                           (expand-file-name user-emacs-directory))
                          "schema/relaxng/xhtml5.rnc"))
          (schema-file (concat (file-name-as-directory
                                (expand-file-name user-emacs-directory))
                               "schema/schemas-html5.xml"))
          (oldschema-file (concat (file-name-as-directory
                                   (expand-file-name user-emacs-directory))
                                  "schema/schemas.xml")))

      (when (locate-library "rng-loc")
        ;; loading "rng-loc" is required, otherwise,
        ;; `rng-schema-locating-files-default' and
        ;; `rng-schema-locating-files' are not defined
        (require 'rng-loc))

      (if (and (file-accessible-directory-p html5-el)
               (file-readable-p schema)
               (file-readable-p schema-file))
          (progn
            (setq rng-schema-locating-files-default
                  (delete "schemas.xml" rng-schema-locating-files))
            (add-to-list 'rng-schema-locating-files schema-file)
            (add-to-list 'rng-schema-locating-files "schemas.xml")
            (add-to-list 'load-path html5-el)
            (when (locate-library "whattf-dt")
              (require 'whattf-dt)))
        (progn
          (setq rng-schema-locating-files-default
                (delete "schemas.xml" rng-schema-locating-files))
          (add-to-list 'rng-schema-locating-files oldschema-file)
          (add-to-list 'rng-schema-locating-files "schemas.xml"))))

    ;; Adding .emacs.d/schema/schemas.xml for schema searching path
    (let ((schema-file (concat (file-name-as-directory
                                (expand-file-name user-emacs-directory))
                               "schema/schemas.xml")))
      (when (and (file-readable-p schema-file)
                 (not (member schema-file rng-schema-locating-files)))
        (setq rng-schema-locating-files-default
              (delete "schemas.xml" rng-schema-locating-files))
        (add-to-list 'rng-schema-locating-files schema-file)
        (add-to-list 'rng-schema-locating-files "schemas.xml")))))


(ad-activate 'nxml-mode)


(defun nxml-enclose-paragraph (start end prefix)
  "Enclose each paragraph with the element in the region.
By default, <para> element is used.  A prefix argument will give you a
chance to change the name of the element."
  (interactive "*r\nP")
  (let (curpos
        (done nil) (elname "para"))
    (if (not (eq (prefix-numeric-value prefix) 1))
        (setq elname (read-string "Element name: "
                                  "para" 'docbook-element-name-history)))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))

        (while (not done)
          (setq curpos (point))
          (forward-paragraph)
          ;;(message (format "curpos(%d) point(%d)" curpos (point)))
          (if (>= curpos (point))
              (progn
                (setq done t)))
          (backward-paragraph)
          (if (eq (char-after) ?\n)
              (goto-char (1+ (point))))

          (if (not done)
              (progn
                (insert (concat "<" elname ">\n"))
                (forward-paragraph)
                (insert (if (eq (char-before) ?\n)
                            (concat "</" elname ">\n")
                          (concat "\n</" elname ">")))
                ;;(message (format "pt(%d) pt-max(%d)" (point) (point-max)))
                (if (>= (point) (1- (point-max)))
                    (setq done t))
                )))))))
