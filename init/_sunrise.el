
(when (locate-library "sunrise-commander")
  (require 'sunrise-commander))

(defun sunrise-cd-frame ()
  "Similar to `sunrise-cd`, except that it switch to the
frame that runs sunrise, if sunrise is running."
  (interactive)
  (if (and (boundp 'sr-running)
           sr-running
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
        (if (and (boundp 'sr-use-other-frame)
                 sr-use-other-frame
                 (not (eq nframe (selected-frame))))
          (let ((buff (find-file-noselect filename nil nil wildcards)))
            (with-selected-frame nframe
              (switch-to-buffer buff))
            (select-frame-set-input-focus nframe))
          (let ((buff (find-file-noselect filename nil nil wildcards)))
            (sr-save-panes-width)
            (sr-quit)
            (set-window-configuration sr-prior-window-configuration)
            (switch-to-buffer buff))))
    (error (message "%s" (cadr description)))))

(global-set-key [(control ?c) (control ?j)] 'sunrise-cd-frame)
(global-set-key [(control ?c) (control ?J)] 'sunrise-cd-frame)

(defvar sr-frame-configuration-support t
  "If nil, do not save/restore frame configuration")

(defvar sr-prior-frame-configuration nil
  "Frame configuration before Sunrise was started.")

(defun cinsk/sr-frame-init (&optional frame)
  "Setup frame for sunrise"
  (when sr-frame-configuration-support
    (let ((width (frame-parameter frame 'width))
          ;; Assuming that fill-column is 70 in most case,
          ;; NEW-WIDTH will be about to 160.
          (new-width (floor (* fill-column (/ 80.0 fill-column) 2))))
      (when (< width new-width)
        (let ((info (wfu/widen-info new-width 'center frame)))
          (modify-frame-parameters frame `((left . ,(car info))
                                           (width . ,(cdr info)))))))))

(defun cinsk/sr-save-frame-configuration ()
  "Save the current frame configuration for `sr-init-hook'."
  (when sr-frame-configuration-support
    (unless sr-prior-frame-configuration
      (setq sr-prior-frame-configuration (current-frame-configuration)))))

(defun cinsk/sr-restore-frame-configuration ()
  "Save the current frame configuration for `sr-quit-hook'."
  (when (and sr-frame-configuration-support sr-prior-frame-configuration)
    (set-frame-configuration sr-prior-frame-configuration)
    (setq sr-prior-frame-configuration nil)))

(eval-after-load "sunrise-commander"
  '(let* ((frwidth (frame-parameter nil 'width))
          (chwidth (/ (frame-pixel-width) frwidth))
          (dpwidth (/ (display-pixel-width) chwidth)))
     (when (> dpwidth (* frwidth 2))
       (add-to-list 'sr-init-hook 'cinsk/sr-frame-init))

     (when (and (boundp 'sr-mode-map) (> dpwidth (* frwidth 3)))
       (define-key sr-mode-map "\C-m" 'sr-advertised-find-file-other-frame)
       (define-key sr-mode-map [return] 'sr-advertised-find-file-other-frame))

     ;; make sure `cinsk/sr-save-frame-configuration' is called *BEFORE*
     ;; `cinsk/sr-frame-init'
     (add-hook 'sr-init-hook #'cinsk/sr-save-frame-configuration)
     (add-hook 'sr-quit-hook #'cinsk/sr-restore-frame-configuration)))
