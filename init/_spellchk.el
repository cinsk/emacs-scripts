;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'transient)

(declare-function flyspell-overlay-p "flyspell")
(declare-function flyspell-region "flyspell")
(declare-function flyspell-buffer "flyspell")
(declare-function flyspell-correct-wrapper "flyspell-correct")

(defvar-local my/flyspell-buffer-checked nil
  "Non-nil once `flyspell-buffer' has been run in this buffer.")

(defun my/flyspell-mark-checked (&rest _)
  (setq my/flyspell-buffer-checked t))

(defun my/flyspell-mark-unchecked (&rest _)
  (setq my/flyspell-buffer-checked nil))

;; Set the flag whenever flyspell-buffer returns normally, however it was
;; called.  A C-g in the middle leaves it nil.
(advice-add 'flyspell-buffer :after #'my/flyspell-mark-checked)

;; Invalidate the flag when earlier results become stale
(add-hook 'flyspell-mode-hook #'my/flyspell-mark-unchecked)
(add-hook 'after-revert-hook #'my/flyspell-mark-unchecked)
(add-hook 'ispell-change-dictionary-hook #'my/flyspell-mark-unchecked)

(defun my--flyspell-has-misspelling-p ()
  (seq-some #'flyspell-overlay-p (overlays-in (point-min) (point-max))))

(defun my--flyspell-on-error-p ()
  "Return non-nil if point is on (or just after) a flyspell overlay."
  (or (seq-some #'flyspell-overlay-p (overlays-at (point)))
      (and (> (point) (point-min))
           (seq-some #'flyspell-overlay-p (overlays-at (1- (point)))))))

(defun my/flyspell-correct-dwim ()
  "Check the buffer if needed, then correct or jump to an error.
If point is on a misspelled word, correct it with
`flyspell-correct-wrapper'.  Otherwise go to the next error."
  (interactive)
  (unless flyspell-mode
    (flyspell-mode 1))			; the mode hook resets the flag
  (unless my/flyspell-buffer-checked
    (flyspell-buffer))            ; the advice sets the flag
  (cond
   ((my--flyspell-on-error-p)
    (call-interactively #'flyspell-correct-wrapper))
   ((my--flyspell-has-misspelling-p)
    (flyspell-goto-next-error))
   (t
    (message "No misspelled words"))))

(defun my/flyspell-change-dictionary (&optional noload)
 "Select a dictionary, then recheck the whole buffer with flyspell.
With a prefix argument (or non-nil NOLOAD), skip the recheck."
  (interactive "P")
  ;; Bind current-prefix-arg to nil so it isn't passed on to
  ;; `ispell-change-dictionary', where it would mean "change globally".
  (let ((current-prefix-arg nil))
    (call-interactively #'ispell-change-dictionary))
  (when (and (not noload) flyspell-mode)
    ;; Drop highlights made with the previous dictionary.
    (flyspell-delete-all-overlays)
    (flyspell-buffer)))

(defun my/flyspell-check-buffer ()
  "Enable flyspell if needed, then check the region or the whole buffer.
With an active region, check only the region with `flyspell-region'.
Otherwise check the whole buffer with `flyspell-buffer'."
  (interactive)
  (unless flyspell-mode
    (flyspell-mode 1))              ; the mode hook resets the flag
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (deactivate-mark)
        (flyspell-region beg end))
    (flyspell-buffer)))             ; the advice sets the flag


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

(when (executable-find "hunspell")
  (setq ispell-program-name "hunspell"
	;; ispell-dictionary "en_US"    ; unset: default coms from LANG
	;; ispell-personal-dictionary nil	; nil is already the default

	;; Having below two setting solves the problem on the
	;; completion problem (<tab> not working) in minibuffer
	;; completion (w/ vertico). I don't know why..
	text-mode-ispell-word-completion nil
	flyspell-use-meta-tab nil)
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flyspell-correct
  :if (executable-find "hunspell")
  :after flyspell
  :bind (([remap ispell-word] . my/flyspell-menu) ; M-$
	 :map flyspell-mode-map
	 ("C-M-'" . flyspell-correct-wrapper))
  :init
  ;; C-, or C-. might collide with other commands. Moving them to
  ;; less-common ones
  (my/move-key :keymap flyspell-mode-map :from "C-," :to "C-M-;")
  (my/move-key :keymap flyspell-mode-map :from "C-." :to "C-M-/"))




