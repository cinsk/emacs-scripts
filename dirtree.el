;;; dirtree.el --- Directory Tree Viewer for Emacs

;; Copyright (C) 2007  Seong-Kook Shin <cinsk at gmail dot com>

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

(defun directory-files-test (directory testproc &optional full match nosort)
  (let ((filelist nil))
    (setq filelist (directory-files directory t match nosort))
    (delq nil (mapcar (lambda (p)
                        (if (funcall testproc p)
                            (if (not (null full)) 
                                p
                              (file-name-nondirectory p))
                          nil))
                      filelist))))
;;;(directory-files-test "~" (lambda (path) (file-directory-p path)) t nil nil)
(defun insert-directory-entry (lst &optional level)
  (let ((lev (or level 0)))
    (save-excursion
      (set-buffer (get-buffer-create "*Tree*"))
      (dolist (elm lst)
        (insert (format "%s [+]-- %s\n" (make-string (* lev 2) ?\ ) elm))))))

;;;(insert-directory-entry '("asdf" "qewr" "zxcv") 3)

(define-derived-mode dirtree-mode nil "DTree"
  "Major mode for directory tree view.
\\{dirtree-mode-map}
"
  (make-local-variable 'dirtree-mode-variant)
  (setq dirtree-mode-variant t)
  (setq buffer-read-only t)
  )

(set-keymap-parent dirtree-mode-map button-buffer-map)
(define-key dirtree-mode-map "n" 'dirtree-next-line)
(define-key dirtree-mode-map "p" 'dirtree-previous-line)
(define-key dirtree-mode-map "\r" 'dirtree-expand-contract)
(define-key dirtree-mode-map "+" 'dirtree-expand)
(define-key dirtree-mode-map "-" 'dirtree-contract)


(defvar default-indent-spaces 2
  "sdf")

(defun line-empty-p ()
  (= (line-beginning-position) (line-end-position)))

(defun dirtree-next-line (&optional arg try-vscroll)
  (interactive "p\np")
  (next-line arg try-vscroll)
  (message "%d: %s" 
           (get-text-property (point) 'level)
           (get-text-property (point) 'fullpath)))

(defun dirtree-previous-line (&optional arg try-vscroll)
  (interactive "p\np")
  (previous-line arg try-vscroll)
  (message "%d: %s" 
           (get-text-property (point) 'level)
           (get-text-property (point) 'fullpath)))

(defun dirtree-set-expand-mark (c)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "\\[.+\\]" (line-end-position) t 1)
        (let ((start (line-beginning-position))
              (end (line-end-position)))
          (replace-match (format "[%c]" c) nil nil)
          (cond ((char-equal c ?+)
                 (put-text-property start end 'expanded :contracted))
                ((char-equal c ?-)
                 (put-text-property start end 'expanded :expanded))
                ((char-equal c ?\ )
                 (put-text-property start end 'expanded :leaf)))))))

(defun dirtree-expand-contract (&optional pos)
  (interactive "d")
  (let ((expanded (get-text-property pos 'expanded)))
    (cond ((eq expanded :expanded) (dirtree-contract pos))
          ((eq expanded :contracted) (dirtree-expand pos)))))

(defun dirtree-contract (&optional pos)
  (interactive "d")
  ;; delete-region start end
  (let ((lev (get-text-property pos 'level))
        (expanded (get-text-property pos 'expanded))
        start end)
    (save-excursion
      (if (eq expanded :expanded)
          (progn
            (forward-line)
            (setq start (point))
            (while (< lev (get-text-property (point) 'level))
              (forward-line))
            (setq buffer-read-only nil)
            (delete-region start (point))
            (goto-char pos)
            (dirtree-set-expand-mark ?+)
            (setq buffer-read-only t)
            ))
      )))

(defun dirtree-expand (&optional pos)
  (interactive "d")
  (let ((path (get-text-property pos 'fullpath))
        (lev (get-text-property pos 'level))
        (expanded (get-text-property pos 'expanded))
        nelems)
    (cond ((string-equal "." (file-name-nondirectory path)) 
           ;; Refresh the current view??
           nil)
          ((string-equal ".." (file-name-nondirectory path)) 
           ;; goto-parent-line??
           nil)
          ((eq expanded :contracted)
           (setq buffer-read-only nil)
           (setq nelems (insert-dir-entries path (+ 1 lev)))
           (if (= nelems 0)
               (dirtree-set-expand-mark ?\ )
             (dirtree-set-expand-mark ?-))
           (setq buffer-read-only t)))))

(defun insert-dir-entries (dir lev)
  "Insert directory entries in the current line. If the
current line is not empty, fill from the next line"
  (let (start end lst (nelems 0))
    (save-excursion
      ;; put-text-property start end prop value &optional object
      (set-buffer (get-buffer-create "*DirTree*"))
      (setq lst (directory-files-test dir
                                      (lambda (path)
                                        (file-directory-p path)) 
                                      t nil nil))
      (dolist (elm lst)
        (and (not (string-equal (file-name-nondirectory elm) "."))
             (not (string-equal (file-name-nondirectory elm) ".."))
             (progn
               (if (not (line-empty-p))
                   (progn (end-of-line)
                          (insert "\n")
                          ))
               (setq start (point))
               (insert (format "%s [+] %s" 
                               (make-string (* default-indent-spaces lev) ?\ ) 
                               (file-name-nondirectory elm)))
               (setq nelems (1+ nelems))
               (setq end (point))
               (put-text-property start end 'fullpath elm)
               (put-text-property start end 'level lev)
               (put-text-property start end 'expanded :contracted)
               ))))
    nelems))

(defun dirtree-view (dir)
  (interactive "DDirectory: ")
  (let ((buf (get-buffer-create "*DirTree*")) flist)
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (setq buffer-read-only nil)
      (insert-dir-entries dir 1)
      (setq buffer-read-only t)
      (if (not (local-variable-p 'dirtree-mode-variant))
          (dirtree-mode))
      (switch-to-buffer buf))))
          

    
