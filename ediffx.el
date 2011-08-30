;;; ediffx.el --- ediff goodies

;; Copyright (C) 2011  Seong-Kook Shin

;; Author: Seong-Kook Shin <cinsky@gmail.com>
;; Keywords: lisp, convenience

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


(eval-when-compile
  (require 'ediff)
  (require 'cl))

(ediff-defvar-local ediffx-truncate-alist nil "")
(ediff-defvar-local ediffx-next-truncates nil "")

;;(define-key ediff-mode-map [?\$] 'ediffx-switch-truncate-lines)

(add-to-list 'ediff-keymap-setup-hook
             (lambda ()
               (define-key ediff-mode-map "$" 'ediffx-switch-truncate-lines)))

(defun ediffx-truncate-status ()
  "Return alist of (BUFFER . TRUNCATE-LINES) of the current ediff buffers"
  (let (result)
    (mapc (lambda (buf)
	    (setq result
		  (cons (cons buf
			      (buffer-local-value 'truncate-lines buf))
			result)))
	  (let (lst)
	    (if (and (local-variable-p 'ediff-buffer-A) ediff-buffer-A)
		(setq lst (cons ediff-buffer-A lst)))
	    (if (and (local-variable-p 'ediff-buffer-B) ediff-buffer-B)
		(setq lst (cons ediff-buffer-B lst)))
	    (if (and (local-variable-p 'ediff-buffer-C) ediff-buffer-C)
		(setq lst (cons ediff-buffer-C lst)))
	    lst))
    (message "ediff-truncate-status: %S" result)
    result))

(defun ediffx-truncate-lines-buffers (mode)
  "Set the `truncate-lines' to all buffers in `ediff-truncate-alist'."
  (if (not (local-variable-p 'ediffx-truncate-alist))
      (progn 
        (make-local-variable 'ediffx-truncate-alist)
        (setq ediffx-truncate-alist (ediffx-truncate-status))))
  (save-current-buffer
    (mapc (lambda (pair)                ; (buffer . truncate-lines)
            (let ((buf (car pair))
                  (status (cdr pair)))
              (set-buffer buf)
              (setq truncate-lines (cond
                                    ((eq mode :on) t)
                                    ((eq mode :off) nil)
                                    ((eq mode :restore) status)))))
          ediffx-truncate-alist)))

(defun ediffx-next-truncate-status ()
  (if (not (local-variable-p 'ediffx-next-truncates))
      (progn
        (make-local-variable 'ediffx-next-truncates)
        (setq ediffx-next-truncates
              '#1=(:truncate :no_truncate :default . #1#))))
  (let ((status (car ediffx-next-truncates)))
    (setq ediffx-next-truncates (cdr ediffx-next-truncates))
    status))

(defun ediffx-switch-truncate-lines ()
  (interactive)
  (let ((action (ediffx-next-truncate-status)))
    (cond ((eq action :truncate)    
           (ediffx-truncate-lines-buffers :on)
           (message "Truncate lines of all buffers"))
          ((eq action :no_truncate)
           (ediffx-truncate-lines-buffers :off)
           (message "Fold lines of all buffers"))
          ((eq action :default)
           (ediffx-truncate-lines-buffers :restore)
           (message "Restore to the previous truncate line mode"))))

  (mapcar (lambda (win)
            (message "set-window-hscroll for win<%S>" win)
            (set-window-hscroll win 0))
          (append
           (if ediff-buffer-A
               (get-buffer-window-list ediff-buffer-A nil t) nil)
           (if ediff-buffer-B
               (get-buffer-window-list ediff-buffer-B nil t) nil)
           (if ediff-buffer-C
               (get-buffer-window-list ediff-buffer-C nil t) nil))))

(provide 'ediffx)
;;; ediffx.el ends here
