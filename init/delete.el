;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;
;;; Text-deleting commands and key bindings
;;;
;;; Commands defined here:
;;;
;;;   `my/kill-same-syntax'            C-S-d, M-z
;;;       Kill the run of characters after point that share one
;;;       syntax class (a word, a run of spaces, a run of punctuation...).
;;;   `my/backward-kill-same-syntax'   (unbound)
;;;       Same, but backward over the characters before point.
;;;   `my/delete-space-forward'        (unbound)
;;;       Delete blank characters (spaces, tabs, full-width spaces...)
;;;       after point, in every major mode.
;;;
;;; The standard `zap-to-char' is moved from M-z to M-Z.
;;;
;;; Related built-in commands, worth knowing before writing new ones:
;;;
;;;   M-\       `delete-horizontal-space'  spaces/tabs on both sides of point
;;;             (C-u M-\ deletes only the ones before point)
;;;   M-SPC     `cycle-spacing'            one space -> no space -> restore;
;;;             customize `cycle-spacing-actions' (Emacs 29+) for more
;;;   C-x C-o   `delete-blank-lines'       collapse blank lines around point
;;;   M-^       `delete-indentation'       join this line to the previous one
;;;   M-d       `kill-word'                like the forward command below, but
;;;             it first skips non-word chars and works on words only
;;;

(require 'thingatpt)                    ; `forward-same-syntax'
(require 'cinsk-common)                 ; `my/move-key' (used at load time)


;;
;; Delete by syntax class
;;
;; These kill (not just delete) text, so that a word removed by mistake
;; can be yanked back with C-y.  Repeated calls append to the same kill
;; ring entry, as `kill-word' does.
;;
;; What counts as a "run" depends on the current major mode's syntax
;; table.  Check a character with `M-x describe-char' or
;; `M-: (string (char-syntax ?-))'.  Notably:
;;
;;   - In `emacs-lisp-mode', `-' has symbol syntax (`_'), not word syntax
;;     (`w'), so "foo-bar" is three runs: "foo", "-", "bar".
;;   - In most programming modes a newline has comment-end syntax (`>'),
;;     so a whitespace run stops at the end of the line.  In
;;     `fundamental-mode', a newline is whitespace, so the run can cross
;;     line breaks.
;;
(defun my/kill-same-syntax (&optional n)
  "Kill forward over N runs of characters that share one syntax class.
A run is a sequence of characters whose syntax class (as returned by
`char-syntax') is the same as that of the first character.  With a
negative N, kill backward instead; see `my/backward-kill-same-syntax'.

Examples in `emacs-lisp-mode' (│ marks point):

  │foo-bar baz      =>  │-bar baz       ; word run
  │   (foo)         =>  │(foo)          ; whitespace run
  │;;; note         =>  │ note          ; punctuation run
  C-u 3 M-z on │foo-bar baz  =>  │ baz  ; \"foo\", \"-\", \"bar\"

Does nothing at the end of the buffer."
  (interactive "p")
  (setq n (or n 1))
  (let ((start (point)))
    ;; `forward-same-syntax' signals an error when it runs into the end
    ;; (or beginning) of the buffer with runs still to go; stop quietly
    ;; there instead and kill whatever was passed over.
    (ignore-errors (forward-same-syntax n))
    (unless (= start (point))
      (kill-region start (point)))))

(defun my/backward-kill-same-syntax (&optional n)
  "Kill backward over N runs of characters that share one syntax class.
This is the mirror of `my/kill-same-syntax': it looks at the character
before point and kills the run that ends at point.

Examples in `emacs-lisp-mode' (│ marks point):

  (foo-bar│ baz)    =>  (foo-│ baz)     ; word run
  (foo   │bar)      =>  (foo│bar)       ; whitespace run

Repeated calls prepend to the same kill ring entry, so the killed
text stays in buffer order.  Does nothing at the beginning of the buffer."
  (interactive "p")
  (my/kill-same-syntax (- (or n 1))))


;;
;; Hungry delete of horizontal whitespace
;;

(defun my/delete-space-forward (&optional arg)
  "Delete blank characters after point, up to the next other character.
Blank characters are those matched by the [:blank:] character class:
space, tab, and Unicode spacing separators such as the no-break space
\(U+00A0) and the full-width ideographic space (U+3000) used in
Korean, Chinese and Japanese text.

Unlike the syntax-based commands above, this behaves the same in every
major mode.  It stops at a newline, so lines are never joined, unless
ARG (the prefix argument, \\[universal-argument]) is non-nil, in which
case newlines are deleted too.

The deleted text is not saved to the kill ring.

Examples (│ marks point):

  foo│  \\tbar               =>  foo│bar
  foo│ FULLWIDTH-SPACE bar  =>  foo│bar
  foo│   NEWLINE bar        =>  foo│NEWLINE bar   ; stops at the line end
    same, with C-u          =>  foo│bar

To delete whitespace on both sides of point, use \\[delete-horizontal-space]."
  (interactive "P")
  (delete-region (point)
                 (progn
                   ;; Character classes go in without the outer
                   ;; brackets here; \n is added only with C-u.
                   ;; [:space:] is avoided on purpose: it follows the
                   ;; mode's syntax table, so it would differ by mode.
                   (skip-chars-forward (if arg "[:blank:]\n" "[:blank:]"))
                   (point))))


;;
;; Key bindings
;;

;; C-c C-d used to be bound to `my/kill-same-syntax', but
;; clojure/cider (and org) use C-c C-d as a prefix key.
(keymap-global-set "C-M-z" #'my/delete-space-forward)

(my/move-key :from "M-z" :to "M-S-z"
             :with #'my/kill-same-syntax)
