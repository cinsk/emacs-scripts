;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(when (and (executable-find "racer")
           (locate-library "racer"))
  ;; (setq racer-cmd "<path-to-racer-srcdir>/target/release/racer")
  ;; (setq racer-rust-src-path "<path-to-rust-srcdir>/src/")

  (with-eval-after-load "rust-mode"
    (add-hook 'rust-mode-hook 'racer-mode)
    (add-hook 'racer-mode-hook 'eldoc-mode)
    ;; In-buffer completion is handled globally by corfu (see vomce.el);
    ;; TAB completes via `tab-always-indent' = 'complete.
    ))
