;; -*-emacs-lisp-*-

;;;
;;; Window and Frame related configuration
;;;


;; Set the default value for the title bar of the Emacs frame.
;;
;; The possible format specifiers (e.g. %F or %b) are explained in
;; the documentation of `mode-line-format'.

(setq frame-title-format
      (if (= (user-uid) 0)
          ;; If Emacs running as root, print "ROOT" just in case
          "%F - ROOT - %b"
        "%F - %b"))

(setq icon-title-format
      (if (= (user-uid) 0)
          "%F - ROOT"
        "%F"))



(defun toggle-current-window-dedication ()
  "Toggle current window as dedicated"
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))
(global-set-key [Scroll_Lock] 'toggle-current-window-dedication)

(defun frame-max-available-width (&optional frame)
  "Return the maximum value for the possible frame width regards
to the display width"
  (let ((width (frame-width frame))
        (char-width (frame-char-width frame))
        (pwidth (frame-pixel-width frame)))
    (- (/ (- (display-pixel-width) (- pwidth (* width char-width)))
          char-width) 2)))

(defun current-frame-configuration-only (&optional frame)
  "Return a list describing the positions and states of FRAME only.

This function behaves similar to `current-frame-configuration'
except the return value contains the information of specified
FRAME only."
  (if (null frame)
      (setq frame (window-frame (selected-window))))
  (let ((fc nil))
    (mapc (lambda (f)
            (if (eq (car f) frame)
                (setq fc f)))
          (cdr (current-frame-configuration)))
    (list 'frame-configuration fc)))


(defun set-this-frame-configuration (configuration)
  "Restore the frame to the state described by CONFIGURATION.

This function behaves similar to `set-frame-configuration' except
it will not affect the other frames that are not described in
CONFIGURATION."
  (let ((old-func (symbol-function 'iconify-frame)))
    (fset 'iconify-frame (lambda (&optional frame) nil))
    (set-frame-configuration configuration t)
    (fset 'iconify-frame old-func)
    nil))


(defun reverse-other-window (arg)
  "Reverse `other-window' with no argument"
  (interactive "p")
  (other-window (- arg)))

(defun first-window ()
  "Select the first window of the current frame."
  (let ((window nil))
    (mapcar '(lambda (w)
               (let ((edges (window-edges w)))
                 (and (eql (car edges) 0)
                      (eql (cadr edges) 0)
                      (setq window w)))) (window-list))
    window))

(defun abs-other-window (index)
  "Same as \\[other-window] except the base is the first window not the
current window"
  (interactive "p")
  (select-window (first-window))
  (other-window index))

(global-set-key [(control x) ?w ?0]
                '(lambda () (interactive) (abs-other-window 0)))
(global-set-key [(control x) ?w ?1]
                '(lambda () (interactive) (abs-other-window 1)))
(global-set-key [(control x) ?w ?2]
                '(lambda () (interactive) (abs-other-window 2)))
(global-set-key [(control x) ?w ?3]
                '(lambda () (interactive) (abs-other-window 3)))
(global-set-key [(control x) ?w ?4]
                '(lambda () (interactive) (abs-other-window 4)))
(global-set-key [(control x) ?w ?5]
                '(lambda () (interactive) (abs-other-window 5)))
(global-set-key [(control x) ?w ?6]
                '(lambda () (interactive) (abs-other-window 6)))
(global-set-key [(control x) ?w ?7]
                '(lambda () (interactive) (abs-other-window 7)))
(global-set-key [(control x) ?w ?8]
                '(lambda () (interactive) (abs-other-window 8)))
(global-set-key [(control x) ?w ?9]
                '(lambda () (interactive) (abs-other-window 9)))

;;(global-set-key [C-tab] 'other-window)  ; C-x o
;;(global-set-key [S-iso-lefttab] 'reverse-other-window)
;;(global-set-key [(backtab)] 'reverse-other-window)
(global-set-key [(control tab)] 'smart-other-frame-or-window)

;; I want C-<tab> works consistently even in minibuffer-mode.
;; Since, C-<tab> is bound to `file-cache-minibuffer-complete'
;; in the minibuffer mode, I'll replace to S-<tab>.
(define-key minibuffer-local-map [(backtab)] 'file-cache-minibuffer-complete)
(define-key minibuffer-local-map [(control tab)] 'smart-other-frame-or-window)

(global-set-key [(control x) ?w ?n] 'other-window)
(global-set-key [(control x) ?w ?o] 'other-window)
(global-set-key [(control x) ?w ?p] 'reverse-other-window)
(global-set-key [(control x) ?w ?k] 'delete-window)
(global-set-key [(control x) ?w ?K] 'delete-other-window)

(defun first-window ()
  "Return the first window of the current frame"
  (labels ((distance (win)
                     (let ((edges (window-edges win)))
                       (+ (car edges) (cadr edges)))))
    (let (cand-win (cand-dist 99999))
      (dolist (win (window-list) cand-win)
        (let ((dist (distance win)))
          (if (< dist cand-dist)
              (setq cand-win win
                    cand-dist dist)))))))

(defun nth-window (n)
  "Select Nth window in cyclic ordering of windows."
  (interactive "P")
  (labels ((position (item seq)
                     (let ((count 0))
                       (catch 'found
                         (dolist (v seq nil)
                           (if (eq v item)
                               (throw 'found count)
                             (setq count (1+ count))))))))
    (let ((pos (if n (prefix-numeric-value n)
                 0)))
      ;; (message "position: %s" pos))
      (other-window (+ pos (let ((winlist (window-list)))
                             ;;(nconc winlist winlist)
                             (position (first-window)
                                       winlist)))))))

(defun smart-other-window ()
  "This calls `other-window' if there are more than one window, otherwise
calls `iswitchb'"
  (interactive)
  (if (one-window-p t 1)
      (call-interactively 'iswitchb-buffer)
    (call-interactively 'other-window)))

(defun smart-other-frame (arg)
  "This calls `other-frame' if there are more than one frame, otherwise calls
`other-window'"
  (interactive "p")
  (if (> (length (frame-list)) 1)
      (other-frame arg)
    (other-window arg)))

(defun smart-other-frame-or-window (&optional arg)
  "Switch focus to other window or frame."
  (interactive "p")
  (if (one-window-p 'nomini)
      (if (> (length (frame-list)) 1)
          (other-frame arg)
        (call-interactively 'iswitchb-buffer))
    (other-window arg)))

(defun reverse-smart-other-frame (arg)
  "This calls `other-frame' if there are more than one frame, otherwise calls
`other-window'"
  (interactive "p")
  (if (> (length (frame-list)) 1)
      (other-frame (- arg))
    (other-window (- arg))))

(global-set-key [(control x) ?o] 'smart-other-frame)
(global-unset-key [(control x) ?f])
(global-set-key [(control x) ?f ?f] 'new-frame)
(global-set-key [(control x) ?f ?k] 'delete-frame)
(global-set-key [(control x) ?f ?K] 'delete-other-frames)
(global-set-key [(control x) ?f ?n] 'smart-other-frame)
(global-set-key [(control x) ?f ?o] 'smart-other-frame)
(global-set-key [(control x) ?f ?p] 'reverse-smart-other-frame)

(defun run-command-other-frame (command)
  "Run COMMAND in a new frame."
  (interactive "CC-x 5 M-x ")
  (select-frame (new-frame))
  (call-interactively command))
(global-set-key "\C-x5\M-x" 'run-command-other-frame)


;;
;; Quick Frame Configuration Load/Save
;;
(global-set-key [(control f3)] '(lambda ()
                                  "Quick frame load"
                                  (interactive)
                                  (jump-to-register ?\x3)
                                  (message "Load saved frame configuration")))

(global-set-key [(control f4)] '(lambda ()
                                  "Quick frame save"
                                  (interactive)
                                  (frame-configuration-to-register ?\x3)
                                  (message "Frame configuration saved")))


(defun frame-position-for-resizing (width height &optional frame display)
  "Return the good frame position (LEFT TOP WIDTH HEIGHT) to satisfy the
new frame size WIDTH and HEIGHT regarding to the current display"
  (or width (setq width (frame-width frame)))
  (or height (setq height (frame-height frame)))
  (let* ((left (frame-parameter frame 'left))
         (top  (frame-parameter frame 'top))
         (margin (frame-margin frame))
         ;; width and height of current frame in characters.
         (cur-width (frame-width frame))
         (cur-height (frame-height frame))
         ;; width and height of the display in pixels.
         (disp-width (display-pixel-width display))
         (disp-height (display-pixel-height display))
         ;; width and height of the character in pixels.
         (char-width (frame-char-width frame))
         (char-height (frame-char-height frame))
         ;; width and height of the new frame size in pixels.
         (pwidth (+ (* char-width width) (car margin)))
         (pheight (+ (* char-height height) (cdr margin))))
    (if (> pwidth disp-width)
        ;; WIDTH is too large for the display.
        (setq width (/ (- disp-width (car margin)) char-width)
              pwidth (+ (* width char-width) (car margin))
              ;; Since `frame-margin' cannot determine the exact
              ;; margin regarding to the window system, it's better to
              ;; set LEFT zero.
              left 0))
    (if (> pheight disp-height)
        ;; HEIGHT is too large for the display.
        (setq height (/ (- disp-height (cdr margin)) char-height)
              pheight (+ (* width char-height) (cdr margin))
              ;; Since `frame-margin' cannot determine the exact
              ;; margin regarding to the window system, it's better to
              ;; set TOP zero.
              top 0))
    (when (> (+ left pwidth) disp-width)
      ;; Current LEFT cannot satisfy WIDTH
      (setq left (- left (+ (* (- width cur-width) char-width)
                            (ceiling (/ (car margin) 2.0))))
            left (if (< left 0) 0 left)))
    (when (> (+ top pheight) disp-height)
      ;; Current LEFT cannot satisfy HEIGHT
      (setq top (- top (+ (* (- height cur-height) char-height)
                          (ceiling (/ (cdr margin) 2.0))))
            top (if (< top 0) 0 top)))
    (list left top width height)))

(defun frame-max-available-width (&optional frame)
  "Return the maximum value for the possible frame width regards
to the display width"
  (let ((width (frame-width frame))
        (char-width (frame-char-width frame))
        (pwidth (frame-pixel-width frame)))
    (- (/ (- (display-pixel-width) (- pwidth (* width char-width)))
          char-width)
       ;; For safety, subtract 2 from the max-width because we don't
       ;; know the exact margin.
       ;;
       ;; TODO: Using zero in MacOS X seems to be fine.  Check in other system.
       0)))


(defun adjacent-window (dir window)
  "Return the adjacent window of the given WINDOW based on the direction, DIR

DIR can be one of 'left, 'right, 'up, 'bottom.  If there is no window
for given DIR, this function returns nil.   If there are more than one
windows toward that DIR, it returns one of the window."
  (let* ((edges (window-edges (selected-window)))
         (left (car edges))
         (top (cadr edges))
         (right (caddr edges))
         (bottom (cadddr edges))
         (vfringe (max (- (window-total-height (selected-window))
                          (window-body-height (selected-window))) 1))
         (hfringe (max (- (window-total-width (selected-window))
                             (window-body-width (selected-window))) 1))
         (point (cond ((eq dir 'left) (cons (- left hfringe) (1+ top)))
                      ((eq dir 'right) (cons (+ right hfringe) (1+ top)))
                      ((eq dir 'up) (cons (1+ left) (- top vfringe)))
                      ((eq dir 'down) (cons (1+ left) (+ bottom vfringe))))))
    ;; I don't know what is the right value for hfringe and vfringe.
    ;; See the Section 28.3 Window Sizes of elisp reference for more.
    (some (lambda (w) w)
          (mapcar (lambda (w)
                    (let ((r (coordinates-in-window-p point w)))
                               (if r w nil)))
                           (window-list)))))

(defun move-window-border-up (&optional amount)
  "Move the window border upward.

If the selected window is the top-most window, then it moves the
bottom border upward. (This causes the selected window shrinks
vertically.)  Otherwise it moves top border upward. (This causes
the selected window grows vertically.)"
  (interactive "p")
  (setq amount (if (eq amount 0) 1 amount))
  (let ((win (adjacent-window 'up (selected-window))))
    (if win
        ;; this is not the top-most window
        (adjust-window-trailing-edge win (- amount))
      ;; this is the top-most window
      (adjust-window-trailing-edge (selected-window) (- amount)))))

(defun move-window-border-down (&optional amount)
  "Move the window border downward.

If the selected window is the bottom-most window, then it moves
the top border downward. (This causes the selected window shrinks
vertically.)  Otherwise it moves bottom border downward. (This
causes the selected window grows vertically.)"
  (interactive "p")
  (setq amount (if (eq amount 0) 1 amount))
  (let ((win (adjacent-window 'down (selected-window))))
    (if win
        ;; this is not the bottom-most window
        (adjust-window-trailing-edge (selected-window) amount)
      ;; this is the bottom-most window
      (window-resize (selected-window) (- amount)))))

(defun move-window-border-left (&optional amount)
  "Move the window border leftward.

If the selected window is the left-most window, then it moves the
right border leftward. (This causes the selected window shrinks
horizontally.)  Otherwise it moves left border leftward. (This
causes the selected window grows horizontally.)"
  (interactive "p")
  (setq amount (if (eq amount 0) 1 amount))
  (let ((win (adjacent-window 'left (selected-window))))
    (message "left: %S" win)
    (if win
        ;; this is not the left-most window
        (adjust-window-trailing-edge win (- amount) t)
        ;; this is the left-most window
      (adjust-window-trailing-edge (selected-window) (- amount) t)
      )))


(defun move-window-border-right (&optional amount)
  "Move the window border rightward.

If the selected window is the right-most window, then it moves
the left border rightward. (This causes the selected window
shrinks horizontally.)  Otherwise it moves right border
rightward. (This causes the selected window grows horizontally.)"
  (interactive "p")
  (setq amount (if (eq amount 0) 1 amount))
  (let ((win (adjacent-window 'right (selected-window))))
    (message "right: %S" win)
    (if win
        ;; this is not the right-most window
        (adjust-window-trailing-edge (selected-window) amount t)
        ;; this is the right-most window
      (window-resize (selected-window) (- amount) 'horizontal))))


(global-set-key [(control ?x) ?w up)] 'move-window-border-up)
(global-set-key [(control ?x) ?w down)] 'move-window-border-down)
(global-set-key [(control ?x) ?w left)] 'move-window-border-left)
(global-set-key [(control ?x) ?w right)] 'move-window-border-right)

(global-set-key [(meta shift up)] 'move-window-border-up)
(global-set-key [(meta shift down)] 'move-window-border-down)
(global-set-key [(meta shift left)] 'move-window-border-left)
(global-set-key [(meta shift right)] 'move-window-border-right)
