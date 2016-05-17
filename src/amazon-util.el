;;; amazon-util.el --- Utility functions for Amazon working environment

;; Copyright (C) 2016  Seong-Kook Shin

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



(defun a/igraph-vertical-line-from-deployment (begin end)
  (interactive "*r")
    (let ((pattern "\\(.*?\\):?[[:blank:]]*\\([0-9]\\{1,2\\}\\)/\\([0-9]\\{1,2\\}\\)/\\([0-9]\\{4\\}\\), *\\([0-9]\\{1,2\\}:[0-9]\\{1,2\\}:[0-9]\\{1,2\\}\\) *\\([AaPp][Mm]\\).*$")
          (local-date-cmd "date -d '%s/%s/%s %s %s'")
          (pst-date-cmd "TZ=America/Los_Angeles date -d \"`%s`\" +'%%Y/%%m/%%d %%I:%%M%%p'")
          pos)
      (save-excursion
        (save-restriction
          (narrow-to-region begin end)
          (goto-char (point-min))
          (when (re-search-forward (concat "^" pattern) nil t)
            (setq pos (copy-marker (match-beginning 0)))
            (let ((ins (format local-date-cmd
                               (match-string-no-properties 4)
                               (match-string-no-properties 2)
                               (match-string-no-properties 3)
                               (match-string-no-properties 5)
                               (match-string-no-properties 6))))
              (let ((replace (save-match-data
                               (replace-regexp-in-string "\n$" "" (shell-command-to-string
                                                                   (format pst-date-cmd ins))))))
                (replace-match (concat "( " (match-string-no-properties 1) " - @ " replace ", ")))))
          (kill-line)
          (when (re-search-forward pattern nil t)
            (let ((ins (format local-date-cmd
                               (match-string-no-properties 4)
                               (match-string-no-properties 2)
                               (match-string-no-properties 3)
                               (match-string-no-properties 5)
                               (match-string-no-properties 6))))
              (let ((replace (save-match-data
                               (replace-regexp-in-string "\n$" "" (shell-command-to-string
                                                                   (format pst-date-cmd ins))))))
                (replace-match (concat (match-string-no-properties 1) " - @ " replace " )"))))
            (when (markerp pos)
              (message "save from %S %S" pos (point))
              (kill-ring-save pos (point))))))))


(provide 'amazon-util)
;;; amazon.el ends here
