;;; _register.el ---                                 -*- lexical-binding: t; -*-


(defun my/insert-register-dwim (register &optional arg)
  "Insert REGISTER's text, or jump to it if it holds anything else.
With prefix ARG, leave point after the inserted text."
  (interactive (list (register-read-with-preview "Register: ")
                     current-prefix-arg))
  (let ((val (get-register register)))
    (if (or (stringp val)
            (numberp val)
            (and (consp val) (stringp (car val))))   ; rectangle
        (insert-register register arg)
      (jump-to-register register))))

(defun my--region-rectangle-p (beg end)
  "Heuristic: return non-nil if BEG..END looks like a rectangle.

The region is considered a rectangle only when all of these hold:
  - it spans more than one line,
  - neither BEG nor END is at the beginning of a line, and
  - BEG and END are in different columns (otherwise the rectangle
    would be empty).

Everything else (single-line regions, whole-line selections, and
mixed cases such as a mid-line start with an end at the beginning
of a line) is considered ordinary text."
  (save-excursion
    (goto-char beg)
    (let ((col-beg (current-column)))
      (and (> end (line-end-position))       ; multi-line
           (not (bolp))                      ; start mid-line
           (progn (goto-char end)
                  (and (not (bolp))          ; end mid-line
                       (/= (current-column) col-beg)))))))

(defun my/copy-to-register-dwim (register &optional arg)
  "Save something into REGISTER, depending on context.

With an active region, save it as text or as a rectangle:
  - If `rectangle-mark-mode' is active, always save a rectangle.
    The prefix argument is ignored in this case.
  - Otherwise, `my--region-rectangle-p' guesses the kind of region.
    Without a prefix argument, the guess is used as is.
    With a prefix argument ARG, the guess is flipped: a region
    that would be text becomes a rectangle, and a region that
    would be a rectangle becomes text.

With no active region, the prefix argument selects what to save:
  - no prefix:                      the window configuration.
  - \\[universal-argument]:                   the frame configuration (all frames).
  - \\[universal-argument] \\[universal-argument]:                 point (as a marker).

Use `my/copy-to-register-dwim' to restore or insert the saved value."
  (interactive (list (register-read-with-preview "Save to register: ")
                     current-prefix-arg))
  (cond
   ((use-region-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (if (or (bound-and-true-p rectangle-mark-mode)
              (xor arg (my--region-rectangle-p beg end)))
          (progn (copy-rectangle-to-register register beg end)
                 (message "Saved rectangle to register %c" register))
        (copy-to-register register beg end)
        (message "Saved text to register %c" register)))
    (deactivate-mark))
   ((equal arg '(16))
    (point-to-register register)
    (message "Saved point to register %c" register))
   (arg
    (frame-configuration-to-register register)
    (message "Saved frame configuration to register %c" register))
   (t
    (window-configuration-to-register register)
    (message "Saved window configuration to register %c" register))))


(my/move-key :from "C-x r w" :to "C-x r W" :with #'my/copy-to-register-dwim)
(my/move-key :from "C-x r y" :to "C-x r Y" :with #'my/insert-register-dwim)

;; <f2> is bound to 2C-comman which I don't use.  Since <f6> is
;; undefined, move <f2> to <f6>.
(my/move-key :from "<f2>" :to "<f6>" :with #'my/insert-register-dwim)
(keymap-global-set "C-<f2>" #'my/copy-to-register-dwim)
