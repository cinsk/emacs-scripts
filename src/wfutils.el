;;; wfutils.el --- Window/Frame convenience utility  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Seong-Kook Shin

;; Author: Seong-Kook Shin <cinsky@gmail.com>
;; Keywords: frames, convenience
;; Package-Requires: ((emacs "29.1"))

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

;; Convenience commands for switching between windows and frames by
;; position or index, for resizing windows from the keyboard, and for
;; resizing the selected window or frame to a given size.
;;
;; Loading this library does not change any key bindings.  To enable
;; them, add this to your init file:
;;
;;   (require 'wfutils)
;;   (wfu-mode 1)
;;
;; Main commands and their bindings in `wfu-mode':
;; - `wfu/other-window-or-frame'  (C-<tab>)
;; - `wfu/other-frame-or-window'  (C-x o)
;; - `wfu/move-window-border-up', `-down', `-left', `-right'
;;   (C-M-S-<arrow>)
;; - `delete-window', `delete-frame', `make-frame'
;;   (C-c k w, C-c k f, C-c k n)
;;
;; `wfu/resize-window' has no default key binding.  It resizes the
;; selected window to a given WIDTH x HEIGHT (in characters):
;;
;; - If the selected window is the only window on its frame, the frame
;;   itself is resized so that its single window ends up WIDTH x HEIGHT.
;; - Otherwise, the enclosing frame is grown first if it is too small to
;;   fit a window of that size given the other windows already on it,
;;   and then the selected window is resized within the frame.
;; - With EXPAND-ONLY non-nil, resizing only ever grows: it never
;;   shrinks the window or frame below their current size.
;;
;; Interactively, the command always prompts for a size (a prefix
;; argument makes no difference).  The prompt accepts:
;;
;;   "WxH"  set the width to W and the height to H,
;;   "W"    set only the width to W, keeping the current height,
;;   "xH"   set only the height to H, keeping the current width,
;;   ""     (just RET) use the default size, 80x25.
;;
;; Resizing a frame only works on graphical displays; on a text
;; terminal the command signals a `user-error' in the single-window
;; case.
;;
;; Naming convention: public names start with `wfu/', private names
;; start with `wfu--'.

;;; Code:

(require 'subr-x)

(defgroup wfutils nil
  "Window and frame convenience utilities."
  :group 'convenience
  :prefix "wfu/")


;;;; Frame geometry

(defun wfu/frame-left (&optional frame)
  "Return the numeric value of the `left' parameter of FRAME."
  (let ((p (frame-parameter frame 'left)))
    (cond ((numberp p) p)
          ((eq (car-safe p) '+) (cadr p))
          ((eq (car-safe p) '-) (- (display-pixel-width frame) (cadr p)))
          (t 0))))

(defun wfu/frame-top (&optional frame)
  "Return the numeric value of the `top' parameter of FRAME."
  (let ((p (frame-parameter frame 'top)))
    (cond ((numberp p) p)
          ((eq (car-safe p) '+) (cadr p))
          ((eq (car-safe p) '-) (- (display-pixel-height frame) (cadr p)))
          (t 0))))

(defvar wfu/frame-ordered-policies
  (list (cons 'x (lambda (f)
                   (+ (* (display-pixel-height f) (wfu/frame-left f))
                      (wfu/frame-top f))))
        (cons 'y (lambda (f)
                   (+ (* (display-pixel-width f) (wfu/frame-top f))
                      (wfu/frame-left f))))
        (cons 'd (lambda (f)
                   (let ((left (wfu/frame-left f))
                         (top (wfu/frame-top f)))
                     (sqrt (+ (* left left) (* top top)))))))
  "Alist of sort policies for `wfu/frame-list-ordered'.

The policy `x' sorts the frames by their X coordinates, the policy
`y' sorts them by their Y coordinates, and the policy `d' sorts
them by their distance from the origin (0, 0) of the display.

Each policy function takes a frame and returns a priority number.
A lower number means a higher priority.")

(defcustom wfu/frame-ordered-policy 'x
  "Default sort policy for `wfu/frame-list-ordered'.

The available policies are defined in `wfu/frame-ordered-policies'."
  :type '(choice (const :tag "By X coordinate" x)
                 (const :tag "By Y coordinate" y)
                 (const :tag "By distance from the origin" d))
  :group 'wfutils)


;;;; Ordered lists

(defun wfu/window-list-ordered (&optional frame)
  "Return a list of the windows of FRAME, in reading order.

The windows are sorted from top to bottom, and from left to right
within the same row.  If FRAME is nil, use the selected frame.
The minibuffer window is not included."
  (let* ((width (frame-width frame))
         (elist (mapcar (lambda (w)
                          (let ((edges (window-edges w)))
                            (cons (+ (nth 0 edges) (* (nth 1 edges) width))
                                  w)))
                        (window-list frame 'nomini))))
    (mapcar #'cdr (sort elist (lambda (lhs rhs) (< (car lhs) (car rhs)))))))

(defun wfu/frame-list-ordered (&optional policy)
  "Return a list of the visible frames, sorted by POLICY.

See `wfu/frame-ordered-policies' for the available values of
POLICY.  If POLICY is nil, use the value of
`wfu/frame-ordered-policy'."
  (let* ((policy (or policy wfu/frame-ordered-policy))
         (func (or (cdr (assq policy wfu/frame-ordered-policies))
                   (error "Unknown frame ordering policy: %S" policy)))
         (elist (mapcar (lambda (f) (cons (funcall func f) f))
                        (visible-frame-list))))
    (mapcar #'cdr (sort elist (lambda (lhs rhs) (< (car lhs) (car rhs)))))))

(defun wfu--nth-by-index (index list)
  "Return the INDEX-th element of LIST, counting from 1.

A negative INDEX counts from the end of LIST, so -1 is the last
element.  An out-of-bounds INDEX wraps around."
  (let ((n (length list)))
    (nth (if (< index 0)
             (mod index n)
           (mod (1- index) n))
         list)))


;;;; Selecting by index

(defun wfu/select-frame-by-index (index)
  "Select the INDEX-th frame and give it the input focus.

The frames are taken from `wfu/frame-list-ordered'.  See
`wfu/select-window-by-index' for how INDEX is interpreted."
  (select-frame-set-input-focus
   (wfu--nth-by-index index (wfu/frame-list-ordered))))

(defun wfu/select-window-by-index (index &optional all)
  "Select the INDEX-th window.

The windows are taken from `wfu/window-list-ordered'.  If ALL is
non-nil, use the windows of all visible frames, in the order of
`wfu/frame-list-ordered'.

Note that the first window has the index 1, not zero.  If INDEX is
negative, the windows are counted in reverse order, so -1 selects
the last window.  An out-of-bounds INDEX wraps around."
  (let* ((windows (if all
                      (apply #'append
                             (mapcar #'wfu/window-list-ordered
                                     (wfu/frame-list-ordered)))
                    (wfu/window-list-ordered)))
         (window (wfu--nth-by-index index windows))
         (frame (window-frame window)))
    (unless (eq frame (selected-frame))
      (select-frame-set-input-focus frame))
    (select-window window)))

(defun wfu--switch-buffer-command ()
  "Return the command to call when there is nothing else to switch to.

This is the remapping of `switch-to-buffer', or the global
binding of \\`C-x b', or `switch-to-buffer' itself."
  (or (command-remapping 'switch-to-buffer)
      (keymap-global-lookup "C-x b")
      'switch-to-buffer))

(defun wfu--select-by-prefix (arg)
  "Select a window or a frame by the index given by the prefix ARG.

A positive index selects a window if the selected frame has more
than one window.  Otherwise, the absolute value of the index
selects a frame."
  (let ((index (prefix-numeric-value arg)))
    (if (and (> index 0) (not (one-window-p 'nomini)))
        (wfu/select-window-by-index index)
      (wfu/select-frame-by-index (abs index)))))


;;;; Commands: switching

(defun wfu/other-frame-or-window (&optional arg)
  "Select another frame, or another window if there is no other frame.

Without a prefix ARG:
- If there is only one frame with a single window, call the command
  bound to \\`C-x b' (normally `switch-to-buffer').
- If the current frame is a tty frame, or only one frame is visible,
  select the next window.
- Otherwise, select the next frame.

With a prefix ARG, let N be its numeric value:
- If the frame has only one window, select the frame with index |N|.
- If N is positive, select the window with index N.
- Otherwise, select the frame with index |N|.

This command is like `wfu/other-window-or-frame', except that it
prefers frames over windows."
  (interactive "P")
  (cond
   ;; Nothing to switch to, so switch the buffer instead.
   ((and (null arg)
         (one-window-p 'nomini)
         (null (cdr (frame-list))))
    (call-interactively (wfu--switch-buffer-command)))
   ;; A tty frame cannot switch to other frames, and M-<TAB> may not
   ;; work there, so check `display-graphic-p' first and fall back to
   ;; `other-window' when frames are not usable.
   ((null arg)
    (if (or (not (display-graphic-p))
            (null (cdr (visible-frame-list))))
        (other-window 1)
      (other-frame 1)))
   (t
    (wfu--select-by-prefix arg))))

(defun wfu/other-window-or-frame (&optional arg)
  "Select another window, or another frame if there is no other window.

Without a prefix ARG:
- If there is only one frame with a single window, call the command
  bound to \\`C-x b' (normally `switch-to-buffer').
- If the selected frame has only one window, select the next frame.
- Otherwise, select the next window.

With a prefix ARG, let N be its numeric value:
- If the frame has only one window, select the frame with index |N|.
- If N is positive, select the window with index N.
- Otherwise, select the frame with index |N|.

For example, the prefix argument 2 selects the second window of
the selected frame.  If the frame has only one window, it selects
the second frame.

This command is like `wfu/other-frame-or-window', except that it
prefers windows over frames."
  (interactive "P")
  (cond
   ((and (null arg)
         (one-window-p 'nomini)
         (null (cdr (frame-list))))
    (call-interactively (wfu--switch-buffer-command)))
   ((null arg)
    (if (one-window-p 'nomini)
        (other-frame 1)
      (other-window 1)))
   (t
    (wfu--select-by-prefix arg))))


;;;; Commands: resizing

(defun wfu/adjacent-window (dir window)
  "Return a window adjacent to WINDOW in the direction DIR.

DIR is one of the symbols `left', `right', `up' or `down'
\(`bottom' is accepted as a synonym of `down').  If there is no
window in that direction, return nil.  If there are several, return
one of them."
  (window-in-direction
   (pcase dir
     ('left 'left)
     ('right 'right)
     ('up 'above)
     ((or 'down 'bottom) 'below)
     (_ (error "Invalid direction: %S" dir)))
   window))

(defun wfu--move-window-border (dir amount)
  "Move a border of the selected window in the direction DIR by AMOUNT.

DIR is one of `up', `down', `left' or `right'.  When the window has
a neighbor in that direction, the border between them moves.
Otherwise, the opposite border of the selected window moves, so the
window shrinks.  An AMOUNT of 0 is treated as 1."
  (unless (one-window-p)
    (let* ((self (selected-window))
           (amount (if (zerop amount) 1 amount))
           (horizontal (and (memq dir '(left right)) t))
           (win (wfu/adjacent-window dir self)))
      (if (memq dir '(up left))
          ;; Move the border toward the top or the left.  Without a
          ;; neighbor, this moves the bottom or right border of the
          ;; selected window.
          (adjust-window-trailing-edge (or win self) (- amount) horizontal)
        ;; Move the border toward the bottom or the right.
        (if win
            (adjust-window-trailing-edge self amount horizontal)
          (window-resize self (- amount) horizontal))))))

(defun wfu/move-window-border-up (&optional amount)
  "Move the window border upward by AMOUNT lines.

If the selected window is the top-most window, move its bottom
border upward, which makes the window shrink vertically.
Otherwise, move its top border upward, which makes the window grow
vertically."
  (interactive "p")
  (wfu--move-window-border 'up amount))

(defun wfu/move-window-border-down (&optional amount)
  "Move the window border downward by AMOUNT lines.

If the selected window is the bottom-most window, move its top
border downward, which makes the window shrink vertically.
Otherwise, move its bottom border downward, which makes the window
grow vertically."
  (interactive "p")
  (wfu--move-window-border 'down amount))

(defun wfu/move-window-border-left (&optional amount)
  "Move the window border leftward by AMOUNT columns.

If the selected window is the left-most window, move its right
border leftward, which makes the window shrink horizontally.
Otherwise, move its left border leftward, which makes the window
grow horizontally."
  (interactive "p")
  (wfu--move-window-border 'left amount))

(defun wfu/move-window-border-right (&optional amount)
  "Move the window border rightward by AMOUNT columns.

If the selected window is the right-most window, move its left
border rightward, which makes the window shrink horizontally.
Otherwise, move its right border rightward, which makes the window
grow horizontally."
  (interactive "p")
  (wfu--move-window-border 'right amount))


;;;; Commands: resizing to a given size

(defconst wfu--default-window-width 80
  "Default width, in columns, used when the size prompt is left empty.")

(defconst wfu--default-window-height 25
  "Default height, in lines, used when the size prompt is left empty.")

(defconst wfu--resize-max-tries 5
  "Maximum number of corrective passes in `wfu--resize-frame-only'.")

(defun wfu--window-cols (window)
  "Return the total width of WINDOW as an integer (rounded down).
`window-total-width' can return a fraction on graphical frames, so
round down to stay on the safe side when checking for shortfalls."
  (window-total-width window 'floor))

(defun wfu--window-lines (window)
  "Return the total height of WINDOW as an integer (rounded down).
See `wfu--window-cols'."
  (window-total-height window 'floor))

(defun wfu--resize-frame-only (frame width height expand-only)
  "Resize FRAME so that its single window is at least WIDTH x HEIGHT.
If EXPAND-ONLY is non-nil, never shrink FRAME below its current size.

The frame is first set to WIDTH x HEIGHT plus the space used outside
the window (the minibuffer, for example).  That space is estimated as
the difference between the frame size and the window size, clamped to
zero because fringes, scroll bars and dividers can make the two values
disagree by a few columns or lines on graphical frames.  Since the
estimate can still be off, a bounded corrective loop then grows the
frame until the window is large enough.  The loop never shrinks the
frame: a window that overshoots slightly is acceptable, one that
undershoots is not."
  (unless (display-graphic-p frame)
    (user-error "Cannot resize the frame on a text terminal"))
  (let* ((window (frame-selected-window frame))
         (start-width  (frame-width frame))
         (start-height (frame-height frame))
         (width-extra  (max 0 (- start-width  (wfu--window-cols window))))
         (height-extra (max 0 (- start-height (wfu--window-lines window))))
         (target-width  (+ width  width-extra))
         (target-height (+ height height-extra)))
    (when expand-only
      (setq target-width  (max target-width  start-width)
            target-height (max target-height start-height)))
    (set-frame-size frame target-width target-height)
    (let ((tries wfu--resize-max-tries))
      (while (and (> tries 0)
                  (or (< (wfu--window-cols window)  width)
                      (< (wfu--window-lines window) height)))
        (setq tries (1- tries))
        (set-frame-size
         frame
         (+ (frame-width frame)  (max 0 (- width  (wfu--window-cols window))))
         (+ (frame-height frame) (max 0 (- height (wfu--window-lines window)))))))))

(defun wfu--ensure-frame-room (window frame width height)
  "Grow FRAME, if necessary, so WINDOW can be resized to WIDTH x HEIGHT.
Only ever grows the frame; never shrinks it.  This is a best-effort
heuristic: when a frame is grown, Emacs itself decides how the extra
space is distributed among the existing windows."
  (let* ((needed-dw (- width  (wfu--window-cols window)))
         (needed-dh (- height (wfu--window-lines window)))
         (avail-dw (if (> needed-dw 0)
                       (window-resizable window needed-dw t)
                     needed-dw))
         (avail-dh (if (> needed-dh 0)
                       (window-resizable window needed-dh nil)
                     needed-dh))
         (deficit-w (max 0 (ceiling (- needed-dw avail-dw))))
         (deficit-h (max 0 (ceiling (- needed-dh avail-dh)))))
    (when (and (or (> deficit-w 0) (> deficit-h 0))
               (display-graphic-p frame))
      (set-frame-size frame
                      (+ (frame-width frame)  deficit-w)
                      (+ (frame-height frame) deficit-h)))))

(defun wfu--grow-window (window delta horizontal expand-only)
  "Resize WINDOW by DELTA lines (or columns, if HORIZONTAL is non-nil).
Use `window-resizable' to clamp DELTA to what is possible without
violating the minimum sizes of other windows.  When EXPAND-ONLY is
non-nil, a negative (shrinking) DELTA is ignored."
  (when (and (not (zerop delta))
             (or (not expand-only) (> delta 0)))
    (let ((actual (window-resizable window delta horizontal)))
      (unless (zerop actual)
        (window-resize window actual horizontal)))))

(defun wfu--resize-window-in-frame (window frame width height expand-only)
  "Resize WINDOW, one of several windows on FRAME, to WIDTH x HEIGHT.
Grow FRAME first if needed; see `wfu--ensure-frame-room'."
  (wfu--ensure-frame-room window frame width height)
  (let ((dw (round (- width  (window-total-width window))))
        (dh (round (- height (window-total-height window)))))
    (wfu--grow-window window dw t   expand-only)
    (wfu--grow-window window dh nil expand-only)))

(defun wfu--read-window-size ()
  "Prompt for a size and return it as a list (WIDTH HEIGHT nil).
The input uses `x' as the field separator:

  \"WxH\"  returns W and H,
  \"W\"    returns W and nil (keep the current height),
  \"xH\"   returns nil and H (keep the current width),
  \"\"     (just RET) returns the default width and height,
          `wfu--default-window-width' and `wfu--default-window-height'.

Signal a `user-error' if the input has another form or if a given
value is not a positive integer."
  (let* ((window (selected-window))
         (input (read-string
                 (format "Resize window to (WxH, W, xH, or RET for %dx%d; current %dx%d): "
                         wfu--default-window-width
                         wfu--default-window-height
                         (wfu--window-cols window)
                         (wfu--window-lines window))))
         (regexp (concat "\\`[ \t]*\\([0-9]*\\)[ \t]*"
                         "\\(?:[xX][ \t]*\\([0-9]*\\)[ \t]*\\)?\\'"))
         (width-str  (and (string-match regexp input)
                          (match-string 1 input)))
         (height-str (and width-str (match-string 2 input)))
         (width  (and width-str  (not (string-empty-p width-str))
                      (string-to-number width-str)))
         (height (and height-str (not (string-empty-p height-str))
                      (string-to-number height-str))))
    (cond
     ((string-blank-p input)
      (list wfu--default-window-width wfu--default-window-height nil))
     ((not (or width height))
      (user-error "Invalid size \"%s\"; expected WxH, W, or xH, e.g. 120x40"
                  input))
     ((or (and width (<= width 0)) (and height (<= height 0)))
      (user-error "Width and height must be positive: \"%s\"" input))
     (t (list width height nil)))))

;;;###autoload
(defun wfu/resize-window (&optional width height expand-only)
  "Resize the selected window to WIDTH columns by HEIGHT lines.

A nil WIDTH or HEIGHT keeps the current width or height.  A
non-nil value must be a positive number.

When the selected window is the only window on its frame, the
frame itself is resized so that this single window ends up with
the requested size.  Otherwise, the enclosing frame is grown
first (if it is too small to fit a window of that size given the
other windows already on it), and then the selected window is
resized to WIDTH x HEIGHT within the frame.

When EXPAND-ONLY is non-nil, the window/frame is only ever grown,
never shrunk: if it is already at least WIDTH x HEIGHT, it is left
as-is.

Interactively, prompt for a size: \"WxH\" sets both dimensions,
\"W\" sets only the width, and \"xH\" sets only the height, e.g.
\"120x40\", \"120\" or \"x40\".  An empty input (just RET) uses the
default size."
  (interactive (wfu--read-window-size))
  (when (or (and width (<= width 0)) (and height (<= height 0)))
    (user-error "Width and height must be positive"))
  (let* ((window (selected-window))
         (frame  (window-frame window))
         (width  (if width  (round width)  (wfu--window-cols window)))
         (height (if height (round height) (wfu--window-lines window))))
    (if (one-window-p t frame)
        (wfu--resize-frame-only frame width height expand-only)
      (wfu--resize-window-in-frame window frame width height expand-only))))


(defun wfu--frame-focus-candidate (frame)
  "Return a frame that should get input focus when FRAME is deleted, or nil.
Only normal frames qualify: visible (not iconified), on the same
terminal as FRAME, not a child frame, and not minibuffer-only."
  (let ((term (frame-terminal frame)))
    (seq-find (lambda (f)
                (and (not (eq f frame))
                     (eq (frame-visible-p f) t)
                     (eq (frame-terminal f) term)
                     (not (frame-parent f))
                     (not (window-minibuffer-p (frame-root-window f)))))
              ;; Walk frames in `next-frame' order, starting after FRAME.
              (let ((fs (frame-list)))
                (append (cdr (memq frame fs))
                        (seq-take-while (lambda (f) (not (eq f frame))) fs))))))

(defun wfu/delete-frame-refocus (&optional no-focus frame)
  "Delete FRAME (default: the selected frame) without losing input focus.

Before deleting, give input focus to another suitable frame, so the
window manager doesn't hand focus to some other application.

With prefix argument NO-FOCUS, skip the handoff and just delete the
frame, like `delete-frame'."
  (interactive "P")
  (let* ((frame (or frame (selected-frame)))
         (target (and (not no-focus)
                      (eq frame (selected-frame))
                      (wfu--frame-focus-candidate frame))))
    (when target
      (select-frame-set-input-focus target))
    (condition-case err
        (delete-frame frame)
      (error (user-error "%s" (error-message-string err))))))

;;;; Minor mode and key bindings

(defvar-keymap wfu-mode-map
  :doc "Keymap for `wfu-mode'.

The \\`C-c k' bindings are added when the mode is enabled, and only
if \\`C-c k' is unused or already a prefix key in the global map."
  "C-<tab>" #'wfu/other-window-or-frame
  "C-x o" #'wfu/other-frame-or-window
  "C-x f w" #'wfu/resize-window
  "C-x f k" #'wfu/delete-frame-refocus
  "C-x f f" #'make-frame
  "C-M-S-<up>" #'wfu/move-window-border-up
  "C-M-S-<down>" #'wfu/move-window-border-down
  "C-M-S-<left>" #'wfu/move-window-border-left
  "C-M-S-<right>" #'wfu/move-window-border-right)

(defun wfu--bind-kill-keys ()
  "Add the \\`C-c k' bindings to `wfu-mode-map', unless \\`C-c k' is taken.

The prefix is used only if it is unbound, or already a prefix map,
in the global keymap.  It would be better not to call this map a
\"kill map\", because it also holds other bindings."
  (let ((existing (keymap-global-lookup "C-c k")))
    (when (or (null existing) (keymapp existing))
      (keymap-set wfu-mode-map "C-c k w" #'delete-window)
      (keymap-set wfu-mode-map "C-c k f" #'delete-frame)
      (keymap-set wfu-mode-map "C-c k n" #'make-frame))))

;;;###autoload
(define-minor-mode wfu-mode
  "Toggle the window and frame key bindings of wfutils.

When enabled, this global minor mode binds:
\\{wfu-mode-map}"
  :global t
  :group 'wfutils
  ;;(when wfu-mode (wfu--bind-kill-keys))
  )


;;;; Frame widening

(defun wfu/widen-info (width &optional pivot frame)
  "Calculate the position of FRAME if it is widened to WIDTH.

Return (LEFT . NEW-WIDTH), where LEFT is the left-most frame
coordinate in pixels, and NEW-WIDTH is the desired width of the
frame in characters.  Note that NEW-WIDTH may be smaller than
WIDTH so that the frame fits the display.

WIDTH is in characters.  The optional PIVOT should be one of the
symbols `left', `right' or `center' (the default), and it governs
the calculation of LEFT.  For example, a `left' PIVOT tries not to
change the left-most position of the frame, whereas a `right'
PIVOT tries not to change the right-most position.  Whatever the
PIVOT is, the frame is always kept inside the display.

If FRAME is nil, use the selected frame."
  (let* ((cw (frame-char-width frame))
         (dpw (display-pixel-width frame))
         (fw (frame-width frame))
         (febw cw)                      ; extra border width
         (fibw (- (frame-pixel-width frame) (* fw cw))) ; internal border
         (half-border (/ febw 2))
         (new-width (if (> (+ (* width cw) febw fibw) dpw)
                        (/ (- dpw fibw febw) cw)
                      width))
         (delta (* (- new-width fw) cw))
         (left (wfu/frame-left frame))
         (left (cond ((eq pivot 'right) (- left delta))
                     ((eq pivot 'left) left)
                     (t (- left (/ delta 2)))))
         (pixel-width (+ (* new-width cw) fibw half-border)))
    (cons (min (max left half-border) (- dpw pixel-width))
          new-width)))

(provide 'wfutils)
;;; wfutils.el ends here
