;;; wfutils.el --- Window/Frame convenience utility

;; Copyright (C) 2013  Seong-Kook Shin

;; Author: Seong-Kook Shin <cinsky@gmail.com>
;; Keywords: frames, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(defvar wfu/frame-ordered-policies
  '((x . (lambda (f)
           (+ (* (display-pixel-height) (wfu/frame-left f))
              (wfu/frame-top f))))
    (y . (lambda (f)
           (+ (* (display-pixel-width) (wfu/frame-top f))
              (wfu/frame-left f))))
    (d . (lambda (f) (let ((left (wfu/frame-left f))
                           (top (wfu/frame-top f)))
                       (sqrt (+ (* left left) (* top top)))))))
  "Policies alist for `wfu/frame-list-ordered'.

The policy function 'x, sorts the frames by the X coordinates,
and the policy function 'y, sorts the frames by the Y
coordinates, and the policy function 'd, sorts the frames by the
distance from the origin (0, 0) of the display.

Note that these functions return a prioity number, where the
lower number has higher priority.")

(defvar wfu/frame-ordered-policy 'x
  "Default sort policy for the `wfu/frame-list-ordered'.

The actual function is defined in `frame-ordered-policies'.")


(defun wfu/frame-left (&optional frame)
  "Return number representation of frame parameter, 'left"
  (let ((p (frame-parameter frame 'left)))
    (cond ((numberp p) p)
          ((eq '+ (car p)) (cadr p))
          ((eq '- (car p)) (- (display-pixel-width) (cadr p))))))

(defun wfu/frame-top (&optional frame)
  "Return number representation of frame parameter, 'top"
  (let ((p (frame-parameter frame 'top)))
    (cond ((numberp p) p)
          ((eq '+ (car p)) (cadr p))
          ((eq '- (car p)) (- (display-pixel-height) (cadr p))))))

(defun wfu/window-list-ordered (&optional frame)
  "Return a ordered list of windows of FRAME.

If FRAME is nil, it is considered as the selected frame."
  (let ((elist (mapcar (lambda (w)
                         (let ((edges (window-edges w)))
                           (list (+ (car edges)
                                    (* (cadr edges)
                                       (frame-parameter frame 'width)))
                                 w)))
                       (window-list frame))))
    (mapcar (lambda (elt)
              (cadr elt))
            (sort elist (lambda (lhs rhs) (< (car lhs) (car rhs)))))))


(defun wfu/frame-list-ordered (&optional policy)
  "Return a list of frames sorted by POLICY.

See `wfu/frame-ordered-policies' for available value for POLICY.  If
POLICY is nil, the value in `wfu/frame-ordered-policy' will be used."
  (or policy
      (setq policy wfu/frame-ordered-policy))
  (let* ((policy (cdr (assoc policy wfu/frame-ordered-policies)))
         (elist (mapcar (lambda (f)
                          (cons (funcall policy f) f))
                        (visible-frame-list))))
    (mapcar (lambda (elt)
              (cdr elt))
            (sort elist (lambda (lhs rhs) (< (car lhs) (car rhs)))))))


(defun wfu/select-frame-by-index (&optional index)
  "Select the INDEX-th frame and give it the focus.

This function calls `wfu/frame-list-ordered' to get the list of
frames, and select/raise the INDEX-th frame.  See
`wfu/select-window-by-index' for interpretation of INDEX."
  ;; (interactive "P")
  ;; (or index
  ;;     (setq index (read-digit-from-minibuffer "window no: ")))
  (let* ((frames (wfu/frame-list-ordered))
         (nwin (length frames)))
    (select-frame-set-input-focus (nth (if (< index 0)
                                           (mod index nwin)
                                         (mod (- index 1) nwin))
                                       frames))))

(defun wfu/select-window-by-index (&optional index all)
  "Select the INDEX-th window.

This function calls `wfu/window-list-ordered' to get the list
of windows, and select the INDEX-th window.

Note that the first window has the index 1, not zero. If INDEX is
negative, it will select a window in reverse order.  For example,
index -1 will select the last window from the list.  Out of bound
value will be rounded accordingly."
  ;; (interactive "P")
  ;; (or index
  ;;     (setq index (read-digit-from-minibuffer "window no: ")))
  (let* ((windows (if (null all)
                      (wfu/window-list-ordered)
                    (apply 'append (mapcar (lambda (f)
                                             (wfu/window-list-ordered f))
                                           (wfu/frame-list-ordered)))))
         (nwin (length windows))
         (window (nth (if (< index 0)
                          (mod index nwin)
                        (mod (- index 1) nwin))
                      windows))
         (frame (window-frame window)))
    (unless (eq frame (selected-frame))
      (select-frame-set-input-focus frame))
    (select-window window)))


(defun wfu/other-frame-or-window (&optional arg)
  "Select a frame or window.

This function will select a next frame if any.   If there is
only one frame, it will select another window.

Note that this function is the same as `wfu/other-window-or-frame'
except it will select a frame rather than a window in general."
  (interactive "P")
  (let ((switchfunc (or (command-remapping 'switch-to-buffer)
                        (lookup-key (current-global-map)
                                    [(control ?x) ?b])
                        'switch-to-buffer)))
    (if (and (null arg)
             (one-window-p 'no-mini-buffer)
             (= (length (frame-list)) 1))
        (call-interactively switchfunc)
      (if (null arg)
          ;; If emacs has a tty frame and other graphical frames, in a
          ;; tty frame, it cannot switch to other frames, and M-<TAB>
          ;; may not work.  So it's best to check `display-graphic-p'
          ;; first, and call `other-window' if it is not a graphical
          ;; frames.
          (if (or (not (display-graphic-p))
                  (eq 1 (length (visible-frame-list))))
              (other-window 1)
            (other-frame 1))
        (let ((index (prefix-numeric-value arg)))
          (if (one-window-p 'nomini)
              (wfu/select-frame-by-index (abs index))
            (if (> index 0)
                (wfu/select-window-by-index index)
              (wfu/select-frame-by-index (abs index)))))))))


(defun wfu/other-window-or-frame (&optional arg)
  "Select a window or frame.

This function will select a window from the current frame.
If the current frame has only one window, this function will
select another frame.

If there is only one frame and if the frame has only one window,
this function will call `switch-to-buffer' or its remapped
function.

Without a prefix argument, this function will call `other-window' to
select a window, or will call `other-frame' to select a frame.

With a prefix argument, this function interprets the prefix
argument as an index from the sorted list of windows (or frames).
This function will try to select a window then a frame on
positive prefix argument.  Negative prefix argument cause a frame
selection directly.

For example, the prefix argument 2 instructs to this function to
select the second window of the frame.  If the current frame has
no window, it will select the second frame."
  (interactive "P")
  (let ((switchfunc (or (command-remapping 'switch-to-buffer)
                        (lookup-key (current-global-map)
                                    [(control ?x) ?b])
                        'switch-to-buffer)))
    (if (and (null arg)
             (one-window-p 'no-mini-buffer)
             (= (length (frame-list)) 1))
        (call-interactively switchfunc)
      (if (null arg)
          (if (one-window-p 'nomini)
              (other-frame 1)
            (other-window 1))
        (let ((index (prefix-numeric-value arg)))
          (if (one-window-p 'nomini)
              (wfu/select-frame-by-index (abs index))
            (if (> index 0)
                (wfu/select-window-by-index index)
              (wfu/select-frame-by-index (abs index)))))))))



(defun wfu/adjacent-window (dir window)
  "Return the adjacent window of the given WINDOW based on the direction, DIR

DIR can be one of 'left, 'right, 'up, 'bottom.  If there is no window
for given DIR, this function returns nil.   If there are more than one
windows toward that DIR, it returns one of the window."
  (let* ((edges (window-edges (selected-window)))
         (left (car edges))
         (top (cadr edges))
         (right (caddr edges))
         (bottom (cadddr edges))
         (vscrolltype (car (frame-current-scroll-bars (window-frame window))))
         (vscroll (cadr (window-scroll-bars window)))
         (vfringe (max (- (window-total-height (selected-window))
                          (window-body-height (selected-window))) 1))
         (hfringe (max (- (window-total-width (selected-window))
                          (window-body-width (selected-window))) 1))
         (point (cond ((eq dir 'left) (cons (- left hfringe) (1+ top)))
                      ((eq dir 'right) (cons (+ right hfringe) (1+ top)))
                      ((eq dir 'up) (cons (if (eq vscrolltype 'left)
                                              (+ 1 left vscroll)
                                            (1+ left))
                                          (- top vfringe)))
                      ((eq dir 'down) (cons (if (eq vscrolltype 'left)
                                                (+ 1 left vscroll)
                                              (1+ left))
                                            (+ bottom vfringe))))))
    ;; I don't know what is the right value for hfringe and vfringe.
    ;; See the Section 28.3 Window Sizes of elisp reference for more.
    (message "point %S" point)
    (cl-some (lambda (w) w)
             (mapcar (lambda (w)
                       (let ((r (coordinates-in-window-p point w)))
                         (if r w nil)))
                     (window-list)))))

(defun wfu/move-window-border-up (&optional amount)
  "Move the window border upward.

If the selected window is the top-most window, then it moves the
bottom border upward. (This causes the selected window shrinks
vertically.)  Otherwise it moves top border upward. (This causes
the selected window grows vertically.)"
  (interactive "p")
  (unless (one-window-p)
    (setq amount (if (eq amount 0) 1 amount))
    (let ((win (wfu/adjacent-window 'up (selected-window))))
      (if win
          ;; this is not the top-most window
          (adjust-window-trailing-edge win (- amount))
        ;; this is the top-most window
        (adjust-window-trailing-edge (selected-window) (- amount))))))

(defun wfu/move-window-border-down (&optional amount)
  "Move the window border downward.

If the selected window is the bottom-most window, then it moves
the top border downward. (This causes the selected window shrinks
vertically.)  Otherwise it moves bottom border downward. (This
causes the selected window grows vertically.)"
  (interactive "p")
  (unless (one-window-p)
    (setq amount (if (eq amount 0) 1 amount))
    (let ((win (wfu/adjacent-window 'down (selected-window))))
      (if win
          ;; this is not the bottom-most window
          (adjust-window-trailing-edge (selected-window) amount)
        ;; this is the bottom-most window
        (window-resize (selected-window) (- amount))))))

(defun wfu/move-window-border-left (&optional amount)
  "Move the window border leftward.

If the selected window is the left-most window, then it moves the
right border leftward. (This causes the selected window shrinks
horizontally.)  Otherwise it moves left border leftward. (This
causes the selected window grows horizontally.)"
  (interactive "p")
  (unless (one-window-p)
    (setq amount (if (eq amount 0) 1 amount))
    (let ((win (wfu/adjacent-window 'left (selected-window))))
      (message "left: %S" win)
      (if win
          ;; this is not the left-most window
          (adjust-window-trailing-edge win (- amount) t)
        ;; this is the left-most window
        (adjust-window-trailing-edge (selected-window) (- amount) t)))))


(defun wfu/move-window-border-right (&optional amount)
  "Move the window border rightward.

If the selected window is the right-most window, then it moves
the left border rightward. (This causes the selected window
shrinks horizontally.)  Otherwise it moves right border
rightward. (This causes the selected window grows horizontally.)"
  (interactive "p")
  (unless (one-window-p)
    (setq amount (if (eq amount 0) 1 amount))
    (let ((win (wfu/adjacent-window 'right (selected-window))))
      (message "right: %S" win)
      (if win
          ;; this is not the right-most window
          (adjust-window-trailing-edge (selected-window) amount t)
        ;; this is the right-most window
        (window-resize (selected-window) (- amount) 'horizontal)))))



(global-set-key [(control tab)] 'wfu/other-window-or-frame)
(global-set-key [(control x) ?o] 'wfu/other-frame-or-window)

(global-set-key [(control meta shift up)] 'wfu/move-window-border-up)
(global-set-key [(control meta shift down)] 'wfu/move-window-border-down)
(global-set-key [(control meta shift left)] 'wfu/move-window-border-left)
(global-set-key [(control meta shift right)] 'wfu/move-window-border-right)

(let ((killmap (lookup-key (current-global-map) [(control ?c) ?k])))
  (when (or (null killmap) (keymapp killmap))
    (global-set-key [(control ?c) ?k ?w] 'delete-window)
    (global-set-key [(control ?c) ?k ?f] 'delete-frame)

    ;; It would be better if we don't call this map a killmap
    ;; as I want to add additional key bindings
    (global-set-key [(control ?c) ?k ?n] 'make-frame)
    ))


(defun wfu/widen-info (width &optional pivot frame)
  "Calculate frame position if the frame need to be expanded to WIDTH.

This function will return (LEFT . NEW-WIDTH) where LEFT is the
left-most frame coordinate in point, and NEW-WIDTH is the desired
width of the frame in characters.  Note that NEW-WIDTH may be
smaller than WIDTH to ensure it will fit to the display device.

WIDTH is in characters, optional PIVOT should be one of left,
right, or center (by default), that govern the calculation of the
LEFT.  For example, Left PIVOT tries not to change left-most
position of the frame, where the right PIVOT tries not to change
the right-most position of the frame.  Note that no matter what
value the PIVOT is, the frame coordinate may fit to the display
device."
  (let* ((cw (frame-char-width frame))
         (dpw (display-pixel-width))
         (fw (frame-width frame))
         (febw cw)
         (fibw (- (frame-pixel-width frame) (* (frame-width frame) cw)))
         (left (frame-parameter frame 'left)))
    (when (> (+ (* width cw) febw fibw) (display-pixel-width))
        (setq width (/ (- dpw fibw febw) cw)))
    (setq left
          (cond ((eq pivot 'right)
                 (- left (* (- width fw) cw)))
                ((eq pivot 'left)
                 left)
                (t ; center
                 (- left (/ (* (- width fw) cw) 2)))))
    (if (< (- left (/ febw 2)) 0)
        (setq left (/ febw 2)))
    (if (> (+ left (+ (* width cw) fibw (/ febw 2)))
           dpw)
        (setq left (- dpw
                      (+ (* width cw) fibw (/ febw 2)))))
    (cons left width)))

(provide 'wfutils)
;;; wfutils.el ends here
