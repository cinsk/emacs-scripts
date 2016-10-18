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


(defmacro a/with-current-environment (envname &rest body)
  "This may be not good idea..."
  (declare (indent 1) (debug t))
  (let ((savedenv (gensym))
        (savedret (gensym)))
    `(let ((,savedenv (getenv ,envname)))
       (setq ,savedret (ignore-errors ,@body))
       (setenv ,envname ,savedenv)
       ,savedret)))



(defun a/parse-date-from-apollo-deployment ()
  ;; Started: July 28, 2016 at 2:11:09 PM GMT+1
  ;; 1        2    3   4       5 6  7  8     9
  (when (re-search-forward "^ *\\([^:]*\\)[: [:blank:]]*\\([A-Z]?[a-z]+\\) +\\([0-9]+\\), +\\([0-9]+\\) +at +\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) +\\([aApP][mM]\\) +GMT\\([-+][0-9]+\\)?" nil t)
    (let* ((dstr (concat (match-string 2) " "
                         (match-string 3) ", "
                         (match-string 4) " at "
                         (match-string 5) ":"
                         (match-string 6) ":"
                         (match-string 7)))
           (partial (parse-time-string dstr)))
      (setcdr (nthcdr 8 partial) (list (* (string-to-int (match-string 9)) 60 60)))
      (save-match-data
        (if (string-match "^[pP][mM]" (match-string 8))
            (let ((hour (+ (nth 2 partial) 12)))
              (setf (nth 2 partial) hour))))
      (append (list (match-beginning 0)
                    (match-end 0)
                    (match-string 1))
              partial))))

(defun a/parse-date-from-apollo-deployment-alt ()
  ;; Started: 7/28/2016, 2:10:42 PM
  ;; 1        2 3  4     5 6  7  8
  (when (re-search-forward "^ *\\([^:]*\\)[: [:blank:]]*\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\), +\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) +\\([aApP][mM]\\)" nil t)
    (list (match-beginning 0)
          (match-end 0)
          (match-string 1)
          (string-to-int (match-string 7))
          (string-to-int (match-string 6))
          (+ (string-to-int (match-string 5))
             (let ((case-fold-search t))
               (if (char-equal ?p (aref (match-string 8) 0)) 12 0)))
          (string-to-int (match-string 3))
          (string-to-int (match-string 2))
          (string-to-int (match-string 4))
          (current-time-zone))))


(defun a/parse-date-from-apollo-deployment-single ()
  ;; Started: Mon Aug 1 14:20:44 2016 GMT
  ;; 1        2   3   4 5  6  7  8    9

  (when (re-search-forward "^ *\\([^:]*\\)[: [:blank:]]*\\([A-Z]?[a-z]+\\) +\\([A-Z]?[a-z]+\\) +\\([0-9]+\\) +\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) \\([0-9]+\\) +GMT\\([-+][0-9]+\\)?" nil t)
    (let* ((dstr (concat (match-string 2) " "
                         (match-string 3) " "
                         (match-string 4) " "
                         (match-string 5) ":"
                         (match-string 6) ":"
                         (match-string 7) " "
                         (match-string 8)))
           (partial (parse-time-string dstr)))
      (setcdr (nthcdr 8 partial) (list (* (string-to-int (or (match-string 9) "")) 60 60)))
      (append (list (match-beginning 0)
                    (match-end 0)
                    (match-string 1))
              partial))))

(defun a/parse-date-from-mcm-utc ()
  ;; Actual Start (UTC): 04-Aug-2016 14:44
  ;; Actual End (UTC): 04-Aug-2016 14:57
  ;; 1                 2  3   4    5  6
  (when (re-search-forward "^ *\\([^:]*?\\) *(UTC) *: *\\([0-9]+\\)-\\([A-Z]?[a-z]+\\)-\\([0-9]+\\) +\\([0-9]+\\):\\([0-9]+\\)" nil t)
    (let* ((dstr (concat (match-string 2) " "
                         (match-string 3) " "
                         (match-string 4) " "
                         (match-string 5) ":"
                         (match-string 6) " Z"))
           (partial (parse-time-string dstr)))
      (append (list (match-beginning 0)
                    (match-end 0)
                    (match-string 1))
              partial))))

(defvar date-parser-list '(a/parse-date-from-apollo-deployment
                           a/parse-date-from-apollo-deployment-alt
                           a/parse-date-from-apollo-deployment-single
                           a/parse-date-from-mcm-utc)
  "Each function should return a parsed date form like

  (BEGIN END LABEL SECONDS MINUTES HOUR DAY MONTH YEAR DOW DST ZONE)

where BEGIN is the position of the match beginning, END is the
position of the match end, and rest of list is the same as
`decode-time' would return.  The function does not need to save
match data, and should return nil if there is no match.")


(defun a/next-date-in-buffer ()
  (let ((pos (point-marker))
        result)
    (dolist (f date-parser-list)
      (let ((ret (funcall f)))
        (when ret
          (setq result (cons ret result))))
      (goto-char pos))
    (when result
      (let ((first-matched (car (sort result (lambda (a b) (< (car a) (car b)))))))
        (goto-char (cadr first-matched))
        first-matched))))


(defun a/parse-dates-buffer ()
  (let (result next)
    (save-excursion
      (save-match-data
        ;; (goto-char (point-min))
        (while (setq next (a/next-date-in-buffer))
          (setq result (cons next result)))
        (nreverse result)))))


(defun a/igraph-vertical-line (begin end)
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (let ((dates (a/parse-dates-buffer))
            ndates)
        (unless (or (null dates)
                    (eq (length dates) 0))
          (kill-region begin end)
          (a/with-current-environment "TZ"
            (setenv "TZ" "America/Los_Angeles")
            (while (> (setq ndates (length dates)) 0)
              (if (= ndates 1)
                  (let ((date (car dates)))
                    (insert (format "%s - @ %s"
                                    (caddr date) ; label
                                    (format-time-string "%Y/%m/%d %l:%M%p" (apply 'encode-time (cdddr date)))))
                    (newline-and-indent)
                    (setq dates nil))

                (let ((date-from (car dates))
                      (date-to   (cadr dates)))
                    (insert (format "(%s - @ %s, %s - @ %s)"
                                    (caddr date-from) ; label
                                    (format-time-string "%Y/%m/%d %l:%M%p" (apply 'encode-time (cdddr date-from)))
                                    (caddr date-to) ; label
                                    (format-time-string "%Y/%m/%d %l:%M%p" (apply 'encode-time (cdddr date-to)))))
                    (newline-and-indent)
                    (setq dates (cddr dates)))))))))))




                 ;; (MyStartText - @ 2016/07/31 11:30pm, MyEndText - @ 2016/08/01 3:30am)
                 ;; MyText - @ 2016/08/01 1:30am
                 ;;



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

(with-eval-after-load "inf-ruby"
  ;; enable brazil irb on host dev-dsk-*.amazon.com
  (add-to-list 'inf-ruby-implementations
               '("brazil" . "brazil-runtime-exec irb -r irb/completion"))
  (when (string-match "\\`dev-dsk..*\\.amazon\\.com\\'" system-name)
    (setq inf-ruby-default-implementation "brazil")))

(provide 'amazon-util)
;;; amazon.el ends here
