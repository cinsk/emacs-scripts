;; -*-emacs-lisp-*-

;;;
;;; diff & ediff customization
;;;

(eval-when-compile
  (require 'ediff-ptch))

;; Note that some external packages loads 'ediff by themselves, such
;; as magit and color-theme.  Since `ediff-make-wide-display-function'
;; should be set before loading `ediff, ediff customization should be
;; placed in the first place. -- cinsk
(setq ediff-make-wide-display-function 'cinsk/ediff-make-wide-display)
(require 'ediff)
(require 'ediff-ptch)

(global-set-key [(meta ?D) ?f]
                (lambda (arg) (interactive "P")
                  (cinsk/select-command arg 'ediff-files 'ediff-files3)))
(global-set-key [(meta ?D) ?b]
                (lambda (arg) (interactive "P")
                  (cinsk/select-command arg 'ediff-buffers 'ediff-buffers3)))
(global-set-key [(meta ?D) ?d]
                (lambda (arg) (interactive "P")
                  (cinsk/select-command arg 'ediff-directories
                                        'ediff-directories3)))
(global-set-key [(meta ?D) ?v] 'ediff-revision)
(global-set-key [(meta ?D) ?r]
                (lambda (arg) (interactive "P")
                  (cinsk/select-command arg 'ediff-regions-wordwise
                                        'ediff-regions-linewise)))
(global-set-key [(meta ?D) ?w]
                (lambda (arg) (interactive "P")
                  (cinsk/select-command arg 'ediff-windows-wordwise
                                        'ediff-windows-linewise)))

(global-set-key [(meta ?D) ?p] 'ediff-patch-file)
(global-set-key [(meta ?D) ?P] 'ediff-patch-buffer)
(global-set-key [(meta ?D) ?m]
                (lambda (arg) (interactive "P")
                  (cinsk/select-command arg 'ediff-merge-revisions
                                        'ediff-merge-revisions-with-ancestor)))
(global-set-key [(meta ?D) (meta ?D)] 'ediff-show-registry)


(defun cinsk/select-command (arg func1 func2)
  "call interactively FUNC1 if ARG is nil, otherwise call FUNC2."
  (call-interactively (if arg func2 func1)))

(defun cinsk/ediff-revision-buffer-p (buf)
  "Return non-nil if BUF is the temporary revision file from `ediff-revision'."
  (and (buffer-file-name buf)
       (string-match "\\`.*~.*~\\'" (file-name-nondirectory
                                     (buffer-file-name buf)))))

(defun cinsk/ediff-janitor ()
  "Delete ediff-related buffers if it is a VC related files."
  (let ((ediff-buffer-A (and (cinsk/ediff-revision-buffer-p ediff-buffer-A)
                             ediff-buffer-A))
        (ediff-buffer-B (and (cinsk/ediff-revision-buffer-p ediff-buffer-B)
                             ediff-buffer-B)))
    ;; TODO: What about ediff-buffer-C?
    (ediff-janitor nil nil)))

;; no effect here
;;(setq ediff-make-wide-display-function 'cinsk/ediff-make-wide-display)

(eval-after-load "ediff"
  '(progn
     ;; I haven't digged much, but restoring the frame is not working
     ;; (esp. on merge session) if `ediff-toggle-wide-display' is
     ;; registered in `ediff-quit-hook'.  Registering
     ;; `ediff-cleanup-hook' solves the problem. -- cinsk

     (add-hook 'ediff-cleanup-hook (lambda ()
                                     (if ediff-wide-display-p
                                         (ediff-toggle-wide-display))))

     ;; Change the algorithm perhaps find a smaller set of changes.
     ;; This makes `diff' slower.
     (setq ediff-diff-options "-d")

     ;; ignore whitespaces and newlines. (can be toggled on/off via `##')
     (setq ediff-ignore-similar-regions t)
     ;; do not create new frame for the control panel
     (setq ediff-window-setup-function 'ediff-setup-windows-plain)
     ;; If nil, ask the user to kill the buffers on exit.
     ;; (setq ediff-keep-variants nil)

     ;; Delete the buffer for the revision files on `ediff-quit'.
     (add-to-list 'ediff-cleanup-hook #'cinsk/ediff-janitor)

     ;; no effect
     ;; (setq ediff-make-wide-display-function 'cinsk/ediff-make-wide-display)
     ))


(defun diff-ediff-patch2 (&optional arg)
  "Call `ediff-patch-file' on the current buffer.

With a prefix argument, ask the user of the option to the patch
command."
  (interactive "P")
  (require 'ediff-ptch)                 ; required for `ediff-patch-options'
  (let ((new-ediff-patch-options
         (if (and arg (= (prefix-numeric-value arg) 4))
             (read-from-minibuffer (format "patch options [%s]: "
                                           ediff-patch-options)
                                   ediff-patch-options nil nil nil
                                   ediff-patch-options)
           ediff-patch-options)))
    (let ((ediff-patch-options new-ediff-patch-options))
      (call-interactively #'diff-ediff-patch))))

(eval-after-load "diff-mode"
  '(progn
     (define-key diff-mode-map [(control ?c) (control ?e)]
       'diff-ediff-patch2)))

(defvar cinsk/ediff-wide-display-policy 'center
  "Policy of the ediff frame resizing

Set to 'center so that `ediff-toggle-wide-display' will expand
the frame with the pivot in the center of the original frame.
'left causes `ediff-toggle-wide-display' will try to keep the
right corder of the original frame.  'right causes to resize with
the left corder unchanged.")

(defadvice ediff-toggle-wide-display (around cinsk/ad-ediff-toggle-wide-display
                                             ())
  (interactive)
  (let ((w (prefix-numeric-value current-prefix-arg))
        (min-width (cond ((window-live-p ediff-window-A)
                          (if (eq ediff-split-window-function
                                  'split-window-vertically)
                              ;; ediff windows splitted like A/B
                              (window-width ediff-window-A)
                            ;; ediff windows splitted like A|B
                            (frame-width (window-frame ediff-window-A))))
                         ((buffer-live-p ediff-buffer-A)
                          (buffer-local-value 'fill-column
                                              ediff-buffer-A))
                         (t (max fill-column 70)))))
    (setq w (max min-width w))
    ;;(message "width: %S" w)

    (let ((cinsk/ediff-wide-window-width w))
      ad-do-it)))

(ad-activate 'ediff-toggle-wide-display)

(defun cinsk/ediff-make-wide-display ()
  "Construct an alist of parameters for the wide display.
Saves the old frame parameters in `ediff-wide-display-orig-parameters'.
The frame to be resized is kept in `ediff-wide-display-frame'.
This function modifies only the left margin and the width of the display.
It assumes that it is called from within the control buffer."
  ;; TODO: try to use `wfu/widen-info' instead of calculate desired coordinate.
  ;;(message "cinsk/ediff-make-wide-display")
  (if (not (fboundp 'ediff-display-pixel-width))
      (error "Can't determine display width"))
  (let* ((frame-A (window-frame ediff-window-A))
         (frame-A-params (frame-parameters frame-A))
         (fw (frame-width frame-A))
         (fpw (frame-pixel-width frame-A))
         (cw (ediff-frame-char-width frame-A))
         (febw cw)                      ; frame external border width
         (fibw (- fpw (* fw cw)))       ; frame internal border width
         desired-fw desired-fpw desired-left)

    (setq ediff-wide-display-orig-parameters
          (list (cons 'left (max 0 (eval (cdr (assoc 'left frame-A-params)))))
                (cons 'width (cdr (assoc 'width frame-A-params))))
          ediff-wide-display-frame frame-A)

    ;;(message "ediff-wide-display-orig-parameters: %S"
    ;;         ediff-wide-display-orig-parameters)

    ;;(message "wide window width: %S" cinsk/ediff-wide-window-width)
    ;;(message "split function: %S" ediff-split-window-function)
    (setq desired-fw (* cinsk/ediff-wide-window-width
                        (if (and (boundp 'ediff-3way-job) ediff-3way-job)
                            3 2)))

    ;; ensure that DESIRED-FW is smaller than the screen size
    (if (> (+ (* desired-fw cw) febw fibw) (ediff-display-pixel-width))
        (setq desired-fw (/ (- (ediff-display-pixel-width) fibw febw) cw)))

    ;;(setq desired-fpw (+ (* desired-fw cw) fbw))
    (setq desired-fpw (* desired-fw cw))
    (let ((left (eval (cdr (assoc 'left frame-A-params)))))
      (cond ((eq cinsk/ediff-wide-display-policy 'left)
             (setq desired-left (- left (* (- desired-fw fw) cw))))

            ((eq cinsk/ediff-wide-display-policy 'right)
             (setq desired-left left))

            (t                          ; center
             (setq desired-left (- left (/ (* (- desired-fw fw) cw) 2)))))

      ;; ensure that the frame will be inside of the display border.
      (if (< (- desired-left (/ febw 2)) 0)
          (setq desired-left (/ febw 2)))

      (if (> (+ desired-left (+ (* desired-fw cw) fibw (/ febw 2)))
             (ediff-display-pixel-width))
          (setq desired-left (- (ediff-display-pixel-width)
                                (+ (* desired-fw cw) fibw (/ febw 2))))))

    ;; (message "resizing WIDTH to %S where LEFT to %S" desired-fw desired-left)
    (modify-frame-parameters
     frame-A `((left . ,desired-left) (width . ,desired-fw)
               (user-position . t)))))
