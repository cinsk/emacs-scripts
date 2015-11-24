;;; vc-dirx.el ---

;; Copyright (C) 2015  Seong-Kook Shin

;; Author: Seong-Kook Shin <cinsky@gmail.com>
;; Keywords:

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

(require 'vc)

(autoload #'svn-status "psvn" "Entry point into svn-status mode" t)
(autoload #'cvs-status "pcvs" "Entry point into cvs mode" t)
(autoload #'git-status "git" "Entry point into git-status mode" t)

(defvar vc/dir-function-alist
  '((Git . (vc/magit-status egg-status git-status))
    (SVN . svn-status)
    (CVS . vc/cvs-status))
  "association list for (VC-SYSTEM . DIR-FUNCTIONS)")

(defun vc/cvs-status (dir)
  (interactive)
  (cvs-status dir (cvs-flags-query 'cvs-status-flags "cvs status flags")))

(defun vc/magit-status (dir)
  ;; If magit-status receives DIR, it ask the confirmation to create a
  ;; directory, which is not what I want.
  (magit-status))

(defun vc/directory-name (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (or (and buffer-file-name
             (file-name-directory buffer-file-name))
        default-directory)))

(defun vc/call-dir-function (backend directory)
  (let ((procs (cdr (assoc backend vc/dir-function-alist))))
    ;; There was a reason that I didn't use `call-interactively' here.
    ;; But I don't remember now.  `cvs-update', for example, need to
    ;; get some parameter. <-- Is this the reason?
    (setq procs (if (listp procs) procs (list procs)))
    (unless (catch 'vc/done
              ;(ignore-errors
                  (dolist (p procs)
                    (funcall p directory)
                    (throw 'vc/done t)));)
      (vc-dir (or directory default-directory)))))


(defun vc/dir (&optional buffer)
  (interactive)
  (let* ((dir (vc/directory-name buffer))
         (backend (ignore-errors
                    (vc-responsible-backend dir))))
    (vc/call-dir-function backend dir)))



(provide 'vc-dirx)
;;; vc-dirx.el ends here
