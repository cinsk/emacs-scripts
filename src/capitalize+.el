;;; capitalize+.el --- Capitalize current word with several strategies

;; Copyright (C) 2014  Seong-Kook Shin

;; Author: Seong-Kook Shin <cinsky@gmail.com>
;; Keywords: abbrev, convenience

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

;; It seems that (require ...) loads only autoloaded functions, not
;; all functions.  Thus, (funcall 'dabbrev-select-buffers-function)
;; might fail unless loaded by (load ...)
(load "dabbrev")

(defvar capitalize+--state)
(make-variable-buffer-local 'capitalize+--state)

(when nil
  (defun capitalize--head (lst &optional n)
    "Return a copy of LST that contains only first N elements."
    (let ((len (length lst)))
      (nbutlast lst (- len (or n 1)))))

  (defun capitalize--sublist (lst begin &optional nelems result)
    "Return a copy of LST that contains first NELEMS elements in circular way

For example, if LST is '(A B C D E), (sublist-circular lst 3) returns
'(C D E A B)."
    (and (or (null nelems) (< nelems 0))
         (setq nelems (length lst)))
    (let ((lst2 (append lst lst)))
      (setq lst2 (nthcdr begin lst2))
      (head lst2 nelems))))


(defun capitalize--candidates (s &optional nelems)
  (let ((result (list (capitalize s) (downcase s))))
    (catch 'done
      (mapc (lambda (buf)
              (with-current-buffer buf
                (let ((case-fold-search t))
                  (save-excursion
                    (goto-char (point-min))
                    (while (re-search-forward (concat "\\b" s "\\b") nil t)
                      (let ((c (buffer-substring-no-properties
                                (- (point) (length s)) (point))))
                        (unless (or (string-equal c s)
                                    (member c result))
                          (setq result (cons c result))
                          (when (and nelems (>= (length result) nelems))
                            (throw 'done t)))))))))
            (cons (current-buffer) (dabbrev--select-buffers))))
    (unless (member (upcase s) result)
      (setq result (cons (upcase s) result)))
    (nreverse result)))

(defun capitalize-word+ ()
  "Toggle case of the current word among all-lowercases,
capitalized, and all-uppercases."
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (when bounds
        (let ((word (buffer-substring-no-properties
                     (car bounds) (cdr bounds)))
              (pt (car capitalize+--state))
              (pos (cadr capitalize+--state))
              (cand (caddr capitalize+--state))
              c)
          (setq c (if (and capitalize+--state
                           (eq (point) (marker-position pt))
                           (string-equal (nth pos cand) word))
                      (progn (setq pos (% (1+ pos) (length cand)))
                             (setcar (cdr capitalize+--state) pos)
                             (nth pos cand))
                    (let ((cand (capitalize--candidates word)))
                      (setq pos (or (position word cand :test 'equal) -1))
                      (setq pos (% (1+ pos) (length cand)))
                      (setq capitalize+--state (list (point-marker) pos cand))
                      (nth pos cand))))
          ;; (message "%S" capitalize+--state)
          (goto-char (car bounds))
          (insert c)
          (delete-region (cdr bounds) (+ (cdr bounds)
                                         (length word))))))))




(when nil
  (defun capitalize--abbrev (s)
    "Find the word like S, but with different case from several buffers"
    (catch 'found
      (mapc (lambda (buf)
              (with-current-buffer buf
                (let ((case-fold-search t))
                  (save-excursion
                    (goto-char (point-min))
                    (while (re-search-forward (concat "\\b" s "\\b") nil t)
                      (let ((c (buffer-substring-no-properties
                                (- (point) (length s)) (point))))
                        (unless (string-equal c s)
                          (throw 'found c))))))))
            (cons (current-buffer) (funcall dabbrev-select-buffers-function)))
      nil))

  (defun capitalize-word+ ()
    "Toggle case of the current word among all-lowercases,
capitalized, and all-uppercases."
    (interactive)
    (save-excursion
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (when bounds
          (let* ((word (buffer-substring-no-properties
                        (car bounds) (cdr bounds)))
                 (down-p (string= (downcase word) word))
                 (up-p (string= (upcase word) word))
                 (fchar (substring-no-properties word 0 1)))
            (goto-char (car bounds))
            (cond (down-p (capitalize-word 1))
                  (up-p (downcase-word 1))
                  (t (let ((abbrev (capitalize--abbrev word)))
                       (if (not abbrev)
                           (upcase-word 1)
                         (insert abbrev)
                         (delete-region (cdr bounds) (+ (cdr bounds)
                                                        (length word))))))))))))
)

(substitute-key-definition 'capitalize-word 'capitalize-word+
                           (current-global-map))

;;(global-set-key [(meta ?C)] 'capitalize-word)
;;(global-set-key [(meta ?c)] 'capitalize-word+)


(provide 'capitalize+)
;;; capitalize+.el ends here
