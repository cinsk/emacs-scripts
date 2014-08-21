
(defvar insertat-overlay-arrow-position
  nil)

(defvar insertat-transform-hook nil
  "Each hook function receives three argumenst; BEGIN END SOURCE.

The inserted text is placed between BEGIN and END.  The text was originally
from SOURCE buffer."
)

(define-globalized-minor-mode global-insertat-mode insertat-mode
  turn-on-insertat-mode)

(defun turn-on-insertat-mode ()
  (insertat-mode 1))

(define-minor-mode insertat-mode
  "Turnon insertat mode; provides a way to insert region to specific point"
  ;; initial value
  nil
  ;; mode line indigator
  " INSat"
  ;; minor mode binding
  '(([(control ?c) (control ?\ )] . insertat-set-anchor)
    ([(control ?c) (control ?y)] . insertat-region)))

(defun insertat-set-anchor (&optional arg)
  (interactive "*P")
  (if arg
      (progn (setq overlay-arrow-variable-list
                   (remq 'insertat-overlay-arrow-position
                         overlay-arrow-variable-list))
             (dolist (w (get-buffer-window-list
                         (marker-buffer insertat-overlay-arrow-position)))
               (if (window-live-p w)
                   (redraw-frame (window-frame w))))
             (setq insertat-overlay-arrow-position nil)
             (message "insertat: anchor removed"))
    (setq insertat-overlay-arrow-position (copy-marker (point) t))
    (add-to-list 'overlay-arrow-variable-list
                 'insertat-overlay-arrow-position)
    (message "insertat: anchor at %d in %s"
             (marker-position insertat-overlay-arrow-position)
             (marker-buffer insertat-overlay-arrow-position))))

;; (defun inserat-transform-hook (buffer begin end src-buffer) ... )

(defun insertat-yank (&optional arg)
  (interactive "*P")
  (if (not (buffer-live-p (marker-buffer insertat-overlay-arrow-position)))
      (yank arg)
    (save-excursion
      (with-current-buffer (marker-buffer insertat-overlay-arrow-position)
        (goto-char insertat-overlay-arrow-position)
        (yank arg)))))

(defun insertat-region (begin end)
  (interactive "r")

  (when (buffer-live-p (marker-buffer insertat-overlay-arrow-position))
    (let ((region-text (buffer-substring begin end))
          (src-buffer (current-buffer)))
      ;;(message "begin(%S) end(%S)" begin end)
      ;;(copy-region-as-kill begin end)
      (save-excursion
        (with-current-buffer (marker-buffer insertat-overlay-arrow-position)
          (goto-char insertat-overlay-arrow-position)
          (let ((begin (point-marker)))
            (insert region-text)
            (save-excursion
              (run-hook-with-args 'insertat-transform-hook
                                  begin (copy-marker (point) t) src-buffer))
            (and (not (bolp)) (newline))))))))

(defun insertat-transform-c-declaration (begin end source)
  ;;(message "begin(%S) end(%S)" begin end)
  (save-restriction
    (narrow-to-region begin end)
    (goto-char begin)
    (unless (looking-at "^[[:space:]\n]*\\<static\\>")
      (when (re-search-forward "^\\([[:space:]\n]*\\)" nil t)
        (delete-region (match-beginning 1) (match-end 1))
        (insert "extern ")))
    (forward-line)
    (join-line))
  (goto-char begin)
  (when (re-search-forward ".*)\\([[:space:]]*\\)$" nil t)
    (goto-char (match-beginning 1))
    (delete-region (match-beginning 1) (match-end 1))
    (insert ";"))
  (indent-region begin end))

(add-hook 'insertat-transform-hook 'insertat-transform-c-declaration)


(provide 'insertat)
