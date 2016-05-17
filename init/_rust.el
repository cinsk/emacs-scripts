
(when (and (executable-find "racer")
           (locate-library "racer")
           (locate-library "company"))
  ;; (setq racer-cmd "<path-to-racer-srcdir>/target/release/racer")
  ;; (setq racer-rust-src-path "<path-to-rust-srcdir>/src/")

  (with-eval-after-load "rust-mode"
    (add-hook 'rust-mode-hook 'racer-mode)
    (add-hook 'racer-mode-hook 'eldoc-mode)

    (add-hook 'rust-mode-hook 'company-mode)
    (setq company-tooltip-align-annotations t)

    (define-key rust-mode-map [tab] 'company-indent-or-complete-common)))
