;;; popup-buffer.el --- Pop up a read-only view-mode buffer, q to quit  -*- lexical-binding: t; -*-

;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience

;;; Commentary:

;; This file provides the macro `with-popup-buffer'.  It fills a buffer by
;; evaluating BODY, displays it with `pop-to-buffer' (or `display-buffer'),
;; and puts it in `view-mode'.  Pressing `q' quits the popup and restores
;; the window state from before it was displayed.  Depending on the :kill
;; argument, the buffer is either killed or kept.
;;
;; Emacs 29.1 or later is needed for `keymap-set'.
;;
;; SYNTAX
;;
;;   (with-popup-buffer (NAME &key MODE ACTION (SELECT t) (KILL t))
;;     BODY...)
;;
;;   NAME     Buffer name (a string form).
;;   :mode    Major mode function to call before BODY, e.g. #'emacs-lisp-mode.
;;            It is called after the buffer is erased and before BODY runs.
;;   :action  ACTION argument passed to `pop-to-buffer' / `display-buffer'.
;;            This form is evaluated, so quote literal actions.
;;   :select  Non-nil (default) selects the popup window.  Nil shows the
;;            buffer without moving focus (`display-buffer' is used).
;;   :kill    Non-nil (default): `q' kills the buffer and closes/restores the
;;            window.  Nil: `q' only closes/restores the window and the
;;            buffer is kept (buried).
;;
;;   BODY is run in the buffer before it is displayed, with the buffer erased
;;   and `inhibit-read-only' bound to t.  Just `insert' text.  Point is moved
;;   to the beginning of the buffer afterwards and the buffer is marked
;;   unmodified.  The macro returns the buffer.
;;
;; EXAMPLES
;;
;; 1. Simplest use.  `q' kills the buffer and restores the windows:
;;
;;      (with-popup-buffer ("*hello*")
;;        (insert "Hello, world!\n"))
;;
;; 2. Two lines of text:
;;
;;      (with-popup-buffer ("*hello*")
;;        (insert "Hello, world!\n")
;;        (insert "This is the second line.\n"))
;;
;; 3. Set a major mode (for syntax highlighting) before filling:
;;
;;      (with-popup-buffer ("*hello-elisp*" :mode #'emacs-lisp-mode)
;;        (insert "(message \"Hello, world!\")\n")
;;        (insert "(+ 1 2)\n"))
;;
;; 4. Keep the buffer on quit.  `q' closes the window only.  Calling the
;;    macro again with the same name refreshes and reuses the buffer.  Use a
;;    leading space in the name to hide it from buffer lists:
;;
;;      (with-popup-buffer ("*hello-keep*" :kill nil)
;;        (insert "Hello, world!\n")
;;        (insert "This buffer survives q.\n"))
;;
;; 5. Bottom strip, 30% of the frame height:
;;
;;      (with-popup-buffer ("*hello*"
;;                          :action '((display-buffer-at-bottom)
;;                                    (window-height . 0.3)))
;;        (insert "Hello from the bottom.\n"))
;;
;; 6. Bottom strip sized to fit the content.  BODY runs before display, so
;;    the content is already there when the window is sized:
;;
;;      (with-popup-buffer ("*hello*"
;;                          :action '((display-buffer-at-bottom)
;;                                    (window-height . fit-window-to-buffer)))
;;        (insert "Hello, world!\n")
;;        (insert "Two lines, so a small window.\n"))
;;
;;    A fixed number of lines also works: (window-height . 10)
;;
;; 7. Split below the selected window:
;;
;;      (with-popup-buffer ("*hello*"
;;                          :action '((display-buffer-below-selected)
;;                                    (window-height . 0.4)))
;;        (insert "Hello, below.\n"))
;;
;; 8. Right-hand split:
;;
;;      (with-popup-buffer ("*hello*"
;;                          :action '((display-buffer-in-direction)
;;                                    (direction . right)
;;                                    (window-width . 0.4)))
;;        (insert "Hello, right.\n"))
;;
;; 9. Side window.  Side windows are not affected by commands like `C-x 1':
;;
;;      (with-popup-buffer ("*hello*"
;;                          :action '((display-buffer-in-side-window)
;;                                    (side . right)
;;                                    (slot . 0)
;;                                    (window-width . 60)))
;;        (insert "Hello, side window.\n"))
;;
;; 10. Reuse the current window.  `q' then restores the previous buffer in
;;     that window instead of closing a window:
;;
;;       (with-popup-buffer ("*hello*"
;;                           :action '((display-buffer-same-window)))
;;         (insert "Hello, same window.\n"))
;;
;; 11. Reuse an existing window if possible, otherwise split.  With a list
;;     of functions, they are tried in order until one succeeds:
;;
;;       (with-popup-buffer ("*hello*"
;;                           :action '((display-buffer-reuse-window
;;                                      display-buffer-pop-up-window)))
;;         (insert "Hello, reused or new window.\n"))
;;
;; 12. Separate frame.  `q' deletes that frame:
;;
;;       (with-popup-buffer ("*hello*"
;;                           :action '((display-buffer-pop-up-frame)
;;                                     (pop-up-frame-parameters
;;                                      . ((width . 80) (height . 30)))))
;;         (insert "Hello, new frame.\n"))
;;
;; 13. Show without selecting.  Focus stays in the original window.  Select
;;     the popup window to press `q' in it:
;;
;;       (with-popup-buffer ("*hello*" :select nil)
;;         (insert "Hello, unfocused.\n"))
;;
;; 14. Keep the display action in a variable:
;;
;;       (defvar my/popup-action
;;         '((display-buffer-at-bottom) (window-height . 0.3)))
;;
;;       (with-popup-buffer ("*hello*" :action my/popup-action)
;;         (insert "Hello, via variable.\n"))
;;
;; 15. Configure placement globally by buffer name, without :action at the
;;     call site.  `pop-to-buffer' consults `display-buffer-alist'.  If both
;;     this and :action match, the `display-buffer-alist' entry is tried
;;     first:
;;
;;       (add-to-list 'display-buffer-alist
;;                    '("\\*hello\\*"
;;                      (display-buffer-at-bottom)
;;                      (window-height . 0.3)))
;;
;;       (with-popup-buffer ("*hello*")
;;         (insert "Hello, placed by display-buffer-alist.\n"))
;;
;; 16. Use the returned buffer:
;;
;;       (let ((buf (with-popup-buffer ("*hello*")
;;                    (insert "Hello, world!\n"))))
;;         (message "Popped up %s" (buffer-name buf)))
;;
;; 17. Pass expressions for :kill, :action, etc.  They are evaluated once,
;;     before the buffer is displayed:
;;
;;       (with-popup-buffer ("*hello*" :kill (not (eq major-mode 'dired-mode)))
;;         (insert "Hello, computed :kill.\n"))
;;
;; NOTES
;;
;; - `q' is bound in `popup-buffer-map', whose parent is `view-mode-map', so
;;   the other view-mode keys (SPC, DEL, s, e, ...) still work.  The map is
;;   installed through `minor-mode-overriding-map-alist', buffer-locally, so
;;   other view-mode buffers are not affected.  To rebind `q' for all popup
;;   buffers, edit `popup-buffer-map'.
;;
;; - Quitting uses `quit-window', which relies on the restore information
;;   that `display-buffer' records on the window.  This undoes whatever
;;   `pop-to-buffer' did: it deletes the window it created or restores the
;;   previous buffer in a reused window.
;;
;; - The major mode is called before BODY, and `popup-buffer-kill-on-quit'
;;   is set afterwards, because a major mode calls `kill-all-local-variables'.
;;
;; - If you change `popup-buffer-map' and re-evaluate its `defvar', use
;;   `eval-defun' (C-M-x), which forces `defvar' forms to reset the value.
;;
;; - To inspect an expansion, put point after a call and run
;;   M-x pp-macroexpand-last-sexp.
;;
;; USAGE IN YOUR CODE
;;
;;   (add-to-list 'load-path "/path/to/dir")
;;   (require 'popup-buffer)

;;; Code:

(require 'cl-lib)
(require 'view)

(defvar-local popup-buffer-kill-on-quit t
  "Non-nil means quitting the popup also kills its buffer.
Nil means only the window is closed (or restored) and the buffer is kept.")

(defun popup-buffer-quit ()
  "Quit the popup window, killing the buffer if `popup-buffer-kill-on-quit'."
  (interactive)
  (quit-window popup-buffer-kill-on-quit))

(defvar popup-buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map view-mode-map)
    (keymap-set map "q" #'popup-buffer-quit)
    map)
  "Keymap overriding `view-mode-map' in popup buffers.")

;;;###autoload
(cl-defmacro with-popup-buffer ((name &key mode action (select t) (kill t))
                                &rest body)
  "Fill buffer NAME by evaluating BODY, then pop it up in `view-mode'.

BODY runs in the (erased, writable) buffer before it is displayed.
Pressing q quits the popup and restores the previous window state.

Keyword args:
  :mode    major mode function to call before BODY (e.g. #\\='emacs-lisp-mode)
  :action  ACTION argument passed to `pop-to-buffer'/`display-buffer'
  :select  non-nil (default) selects the new window
  :kill    non-nil (default) kills the buffer on quit; nil keeps the
           buffer and only closes/restores the window

Returns the buffer.

See the Commentary section of popup-buffer.el for examples."
  (declare (indent 1) (debug ((form &rest form) body)))
  (let ((buf-sym (make-symbol "buf"))
        (kill-sym (make-symbol "kill")))
    `(let ((,buf-sym (get-buffer-create ,name))
           (,kill-sym ,kill))
       (with-current-buffer ,buf-sym
         (let ((inhibit-read-only t))
           (erase-buffer)
           ,@(when mode `((funcall ,mode)))
           ,@body
           (goto-char (point-min))
           (set-buffer-modified-p nil))
         (view-mode 1)
         (setq-local popup-buffer-kill-on-quit ,kill-sym)
         (setq-local minor-mode-overriding-map-alist
                     (list (cons 'view-mode popup-buffer-map))))
       (if ,select
           (pop-to-buffer ,buf-sym ,action)
         (display-buffer ,buf-sym ,action))
       ,buf-sym)))

(provide 'popup-buffer)

;;; popup-buffer.el ends here
