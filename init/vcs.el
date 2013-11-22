;; -*-emacs-lisp-*-

;;;
;;; VCS(Version Control System) related configuration
;;;

;;
;; CVS
;;
(defun pop-to-cvs-buffer (arg)
  "Select \"*cvs*\" buffer in some window, preferably a different one.
If the buffer is not found, call `cvs-examine' interactively.
With a prefix argument, call `cvs-examine' with the prefix argument, 16."
  (interactive "P")
  (let ((buf (get-buffer "*cvs*")))
    (if arg
        (let ((prefix-arg '(16)))       ; C-u C-u
          (call-interactively #'cvs-examine))
      (if buf
          (pop-to-buffer buf)
        (call-interactively #'cvs-examine)))))

;;(global-set-key [f2] #'pop-to-cvs-buffer)


;;
;; Git
;;
(eval-after-load "git"
  '(progn
     (setq git-show-uptodate t
           git-show-ignored t
           git-show-unknown t)
     (define-key git-status-mode-map [(meta ?u)] 'git-refresh-status)))

(when nil
  ;; Currently, I'm using magit, not git package.
  ;; (require 'git) will consume about 4 second(s) in my system.
  (when (locate-library "git")
    (require 'git)))

(let ((magit-git-dir (concat (expand-file-name user-emacs-directory)
                             "magit")))
  (when (file-accessible-directory-p magit-git-dir)
    (add-to-list 'load-path magit-git-dir)
    (add-to-list 'Info-directory-list magit-git-dir))

  (cond ((locate-library "50magit")
         (require '50magit))
        ((locate-library "magit")
         (require 'magit))))

(when (locate-library "markdown-mode")
  ;; See https://github.com/magit/magit/issues/424

  ;; If we don't force to load the module magit here,
  ;; `magit-log-edit-mode' will not have correct keymaps.  Don't know
  ;; why. -- cinsk
  (require 'magit)

  (require 'markdown-mode)
  (define-derived-mode magit-log-edit-mode markdown-mode "Magit Log Edit"))

(when nil
  ;; I do not use egg anymore.
  (let ((egg-dir (concat (file-name-as-directory
                          (expand-file-name user-emacs-directory)) "egg")))
    (if (file-accessible-directory-p egg-dir)
        (progn
          (add-to-list 'load-path egg-dir)
          (when (locate-library "egg")
            (require 'egg))))))

;;
;; vc-jump
;;
(when (locate-library "vc-jump")
  (require 'vc-jump)
  ;; I prefer magit over egg, egg over git
  ;; (add-to-list 'vc-status-assoc
  ;;              (cons 'Git
  ;;                    (cond ((fboundp 'magit-status) #'magit-status)
  ;;                          ((fboundp 'egg-status) #'egg-status)
  ;;                          (#'git-status))))
  (global-set-key [f12] 'vc-jump)
  (global-set-key [(control ?x) ?j] 'vc-jump))

