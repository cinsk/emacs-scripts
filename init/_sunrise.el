
(require 'sunrise-commander)

(defun sunrise-cd-frame ()
  "Similar to `sunrise-cd`, except that it switch to the
frame that runs sunrise, if sunrise is running."
  (interactive)
  (if (and sr-running
           (not (eq (window-frame sr-left-window)
                    (selected-frame))))
      (let ((dir (or (buffer-file-name) (sr-choose-cd-target))))
        (message "dir: %s" dir)
        (with-selected-frame (window-frame sr-left-window)
          (sr-dired dir))
        (select-frame-set-input-focus (window-frame sr-left-window)))
    (sunrise-cd)))

(defun sr-advertised-find-file-other-frame (&optional filename)
  (interactive)
  (let ((sr-use-other-frame t))
    (call-interactively 'sr-advertised-find-file)))

(defun sr-find-regular-file (filename &optional wildcards)
  "Deactivate Sunrise and visit FILENAME as a regular file with WILDCARDS.
\(See `find-file' for more details on wildcard expansion.)"
  (condition-case description
      (let ((nframe (next-frame (selected-frame) 'visible)))
        (if (or (not (boundp 'sr-use-other-frame))
                (null sr-use-other-frame)
                (eq nframe (selected-frame)))
          (let ((buff (find-file-noselect filename nil nil wildcards)))
            (sr-save-panes-width)
            (sr-quit)
            (set-window-configuration sr-prior-window-configuration)
            (switch-to-buffer buff))
          (let ((buff (find-file-noselect filename nil nil wildcards)))
            (with-selected-frame nframe
              (switch-to-buffer buff))
            (select-frame-set-input-focus nframe))))
    (error (message "%s" (cadr description)))))

(defun sr-frame-init (&optional frame)
  "Setup frame for sunrise"
  (set-frame-parameter frame 'width 160))

(global-set-key [(control ?c) (control ?j)] 'sunrise-cd-frame)

(let* ((frwidth (frame-parameter nil 'width))
       (chwidth (/ (frame-pixel-width) frwidth))
       (dpwidth (/ (display-pixel-width) chwidth)))
  (when (> dpwidth (* frwidth 2))
    (add-to-list 'sr-init-hook 'sr-frame-init))
  (when (> dpwidth (* frwidth 3))
    (define-key sr-mode-map "\C-m" 'sr-advertised-find-file-other-frame)))
