;; -*-emacs-lisp-*-

;;;
;;; Maven
;;;

(unless (boundp 'compilation-error-regexp-alist-alist)
  (require 'compile))

(when (not (assoc 'maven3 compilation-error-regexp-alist-alist))
  ;; On GNU Emacs 24.0.94.1 (x86_64-apple-darwin, NS
  ;; apple-appkit-1038.36) of 2012-02-28 on bob.porkrind.org,
  ;; http://emacsformacosx.com/builds/Emacs-pretest-24.0.94-universal-10.6.8.dmg,
  ;; maven in `compilation-error-regexp-alist-alist' is not handling
  ;; maven 3.0.3 in my Macbook.  -- cinsk

  (add-to-list 'compilation-error-regexp-alist-alist
               '(maven3 "^\\[ERROR\\] +\\(.+?\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\].*"
                       1 2 2))
  (add-to-list 'compilation-error-regexp-alist 'maven3)
  ;// "[ERROR] /Users/.../KafkaBridge.java:[71,52] ';' expected"
  )
