;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'transient)

(defvar flyspell-mode)

(declare-function flyspell-overlay-p "flyspell")
(declare-function flyspell-region "flyspell")
(declare-function flyspell-buffer "flyspell")
(declare-function flyspell-prog-mode "flyspell")
(declare-function flyspell-delete-all-overlays "flyspell")
(declare-function flyspell-correct-wrapper "flyspell-correct")
(declare-function ispell-valid-dictionary-list "ispell")
(declare-function ispell-set-spellchecker-params "ispell")
(declare-function ispell-hunspell-add-multi-dic "ispell")
(declare-function ispell-change-dictionary "ispell")

(defvar-local my/flyspell-buffer-checked nil
  "Non-nil once `flyspell-buffer' has been run in this buffer.")

(defun my--flyspell-mark-checked (&rest _)
  (setq my/flyspell-buffer-checked t))

(defun my--flyspell-mark-unchecked (&rest _)
  (setq my/flyspell-buffer-checked nil))

;; Set the flag whenever flyspell-buffer returns normally, however it was
;; called.  A C-g in the middle leaves it nil.
(advice-add 'flyspell-buffer :after #'my--flyspell-mark-checked)

;; Invalidate the flag when earlier results become stale
(add-hook 'flyspell-mode-hook #'my--flyspell-mark-unchecked)
(add-hook 'after-revert-hook #'my--flyspell-mark-unchecked)
(add-hook 'ispell-change-dictionary-hook #'my--flyspell-mark-unchecked)

(defun my--flyspell-ensure ()
  "Enable flyspell in this buffer if it is not already on.
In programming buffers use `flyspell-prog-mode', so that only comments
and strings are checked, not code identifiers."
  (unless flyspell-mode
    ;; Either way the mode hook resets the checked flag.
    (if (derived-mode-p 'prog-mode)
        (flyspell-prog-mode)
      (flyspell-mode 1))))

(defun my--flyspell-on-error-p ()
  "Return non-nil if point is on (or just after) a flyspell overlay."
  (or (seq-some #'flyspell-overlay-p (overlays-at (point)))
      (and (> (point) (point-min))
           (seq-some #'flyspell-overlay-p (overlays-at (1- (point)))))))

(defun my--flyspell-next-error-pos ()
  "Return the start of the next flyspell error after point, or nil.
Wrap around to the first error in the buffer if there is none after
point.  Return nil only when the buffer has no flyspell overlays."
  (let (after first)
    (dolist (o (overlays-in (point-min) (point-max)))
      (when (flyspell-overlay-p o)
        (let ((s (overlay-start o)))
          (when (or (null first) (< s first))
            (setq first s))
          (when (and (> s (point)) (or (null after) (< s after)))
            (setq after s)))))
    (or after first)))

(defun my/flyspell-correct-dwim ()
  "Check the buffer if needed, then correct or jump to an error.
If point is on a misspelled word, correct it with
`flyspell-correct-wrapper'.  Otherwise go to the next error, wrapping
around at the end of the buffer."
  (interactive)
  (my--flyspell-ensure)
  (unless my/flyspell-buffer-checked
    (flyspell-buffer))                  ; the advice sets the flag
  (if (my--flyspell-on-error-p)
      (call-interactively #'flyspell-correct-wrapper)
    (let ((pos (my--flyspell-next-error-pos)))
      (if (null pos)
          (message "No misspelled words")
        (when (< pos (point))
          (message "Wrapped to first error"))
        (goto-char pos)))))

(defun my/ispell-change-dictionary (dict &optional arg)
  "Like `ispell-change-dictionary', but allow selecting multiple dictionaries.
Interactively, prompt with `completing-read-multiple' over the candidates
from `ispell-valid-dictionary-list', join the chosen names with commas,
and pass the result as DICT.  ARG is passed through unchanged."
  (interactive
   (progn
     (require 'ispell)
     (let ((chosen (completing-read-multiple
                    "Use new dictionaries: "
                    (ispell-valid-dictionary-list) nil t)))
       (list (string-join chosen ",")
             current-prefix-arg))))
  (when (and (string-match-p "," dict)
             (fboundp 'ispell-hunspell-add-multi-dic))
    ;; Must run first so hunspell's dictionary alist is initialized.
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic dict))
  (ispell-change-dictionary dict arg))

(defun my/flyspell-change-dictionary (&optional noload)
  "Select a dictionary, then recheck the whole buffer with flyspell.
With a prefix argument (or non-nil NOLOAD), skip the recheck."
  (interactive "P")
  ;; Bind current-prefix-arg to nil so it isn't passed on to
  ;; `ispell-change-dictionary', where it would mean "change globally".
  (let ((current-prefix-arg nil))
    (call-interactively #'my/ispell-change-dictionary))
  (when (and (not noload) flyspell-mode)
    ;; Drop highlights made with the previous dictionary.
    (flyspell-delete-all-overlays)
    (flyspell-buffer)))

(defun my/flyspell-check-buffer ()
  "Enable flyspell if needed, then check the region or the whole buffer.
With an active region, check only the region with `flyspell-region'.
Otherwise check the whole buffer with `flyspell-buffer'."
  (interactive)
  (my--flyspell-ensure)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (deactivate-mark)
        (flyspell-region beg end))
    (flyspell-buffer)))                 ; the advice sets the flag

;; NOTE: the "C-M-..." lines below are plain hints, not menu items.  Keep
;; them in sync with the `my/move-key' calls and the :bind in the
;; flyspell-correct `use-package' form further down.
(transient-define-prefix my/flyspell-menu ()
  "Flyspell menu"
  [[ "Actions"
     ("d" "correct (check first if needed)" my/flyspell-correct-dwim)
     ("g" "check spelling for the region (or whole buffer)" my/flyspell-check-buffer)
     ("c" "change dictionary" my/flyspell-change-dictionary)
     ("u" "toggle flyspell mode" flyspell-mode)
     ("q" "Quit Menu" transient-quit-one) ]
   [ "Global Bindings"
     "C-M-;   go to next error."
     "C-M-'   correct error."
     "C-M-/   auto-correct." ]])

(defvar my/hunspell-dictionaries '("en_US")
  "List of hunspell dictionary names for multi-dictionaries")

(when (executable-find "hunspell")
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flyspell
  :ensure nil                           ; built in
  :if (executable-find "hunspell")
  :defer t
  :init
  (setq ispell-program-name "hunspell"
        ;; ispell-dictionary "en_US"    ; unset: default comes from LANG
        ;; ispell-personal-dictionary nil ; nil is already the default

        ;; These two settings fix <tab> not working in minibuffer
        ;; completion (with vertico).  Reason unknown.
        text-mode-ispell-word-completion nil
        flyspell-use-meta-tab nil)
  :config
  (ispell-set-spellchecker-params)
  (let ((dicts (string-join my/hunspell-dictionaries ",")))
    (ispell-hunspell-add-multi-dic dicts)
    (setq ispell-dictionary dicts)))

(use-package flyspell-correct
  :if (executable-find "hunspell")
  :after flyspell
  :bind (([remap ispell-word] . my/flyspell-menu) ; M-$
         :map flyspell-mode-map
         ("C-M-'" . flyspell-correct-wrapper))
  :init
  ;; C-, or C-. might collide with other commands.  Move them to
  ;; less-common ones.
  (my/move-key :keymap flyspell-mode-map :from "C-," :to "C-M-;")
  (my/move-key :keymap flyspell-mode-map :from "C-." :to "C-M-/"))
