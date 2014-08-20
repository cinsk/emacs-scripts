;;;
;;; rfc2org.el --- Convert text RFC document to ORG format.
;;;

;;;
;;; Copyright (c) 2006  Seong-Kook Shin <cinsky@gmail.com>
;;;

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;; 02111-1307 USA
;;;

;;;
;;; $Id$
;;;

(defvar rfc2org-page-delimiter ""
  "Page delimiters used to convert RFC to ORG format")

(defvar table-of-contents nil
  "dictionary of TOC (obsolete)"))


(defun rfc2org-buffer (src dst)
  (interactive "*bInput RFC buffer: \nMOutput ORG buffer name [.org]: ")
  (let ((buf (get-buffer src)))
    (setq dst (target-buffer-name buf dst))
    (save-excursion
      (set-buffer buf)
      (set-visited-file-name dst)
      (remove-page-delimiters buf)
      (linkify-toc buf t)
      (linkify-referers buf)
      (insert-anchors buf)
      (org-mode))))


(defun remove-page-delimiters (buffer)
  (set-buffer buffer)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "\n\n\n.*\n\n.*\n\n\n" nil t)
        (replace-match "" t t)))))


(defun generate-toc (buffer)
  "Generate TOC alist (obsolete)"
  (let ((old-case case-fold-search)
        (begin 0) (end 0)
        (counter 0)
        entry)
    (set-buffer (get-buffer "rfc2616.txt"))
    (save-excursion
      (save-restriction
        (setq case-fold-search t)
        (widen)
        (goto-char (point-min))
        (setq begin (re-search-forward "Table of Contents"))
        (setq end (re-search-forward "^[^ \n]+.*$" nil t))
        (beginning-of-line)
        (setq end (1- (point)))
        (setq case-fold-search old-case)
        (if (or (= begin 0) (= end 0))
            nil
          (progn
            (narrow-to-region begin end)
            (goto-char (point-min))

            (while (setq entry (tokenize-tocline))
              (setq counter (1+ counter))
              (setq table-of-contents
                    (append table-of-contents (cons entry nil))))
            counter))))))


(defun linkify-referer (buffer)
  "Linkify a section reference (obsolete)
For example:
  \"See section 3.5 for more\" => \"See section [[3.5]] for more\""
  (let ((counter 0))
    (set-buffer buffer)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))

        (while (re-search-forward
                "\\([sS]\\)ection\\([ \n\t]*\\)\\([0-9]+\\(?:\\.[0-9]+\\)*\\)" 
                nil t)
          ;;(match-string 1))))
          (replace-match "\\1ection\\2[[\\3]]" nil nil)
          (setq counter (1+ counter)))
        (message "Linkified %d item(s)" counter)
        counter))))


(defun linkify-referers (buffer &optional strict)
  "Linkify a section reference (obsolete)
If STRICT is not nil, this function perform a job of  `linkify-referer' too.
For example:
  \"Sections 3.5, 4, and, 9.\" => \"Section [[3.5]], [[4]], and, [[9]].\"
  \"Section 3.5, 4, and, 9.\" => \"Section [[3.5]], [[4]], and, [[9]].\""
  (setq strict (if strict t nil))
  (let ((counter 0)
        (section-regexp (if strict 
                            "[sS]ections[ \n\t]*"
                          "[sS]ections?[ \n\t]*")))
    (set-buffer buffer)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))

        (let (beg (end (make-marker)) pos)
          (while (setq beg (re-search-forward section-regexp nil t))
            (forward-paragraph)
            (set-marker end (point))
            
            (goto-char beg)
            (message "beg:end = %d:%d" beg (marker-position end))
            (while (re-search-forward "\\([0-9]+\\(?:\\.[0-9]+\\)*\\)" end t)
              (replace-match "[[\\1]]")
              (setq counter (1+ counter)))
            (goto-char end))
          (message "Linkified %d item(s)" counter)
          counter)))))


(defun linkify-toc (buffer &optional insert-pager)
"Linkify the section number in TOC line.
If INSERT-PAGER is not nil, this function inserts page delimeters on both
sides of TOC block.  See `rfc2org-page-delimiter'."

  (setq insert-pager (if insert-pager t nil))
  (save-excursion
    (save-restriction
      (let ((begin 0) (end nil) (counter 0))
        (set-buffer (get-buffer buffer))
        (widen)
        (goto-char (point-min))
        (setq begin (re-search-forward "Table of Contents" nil t))
        (setq end (re-search-forward "^[^ \n]+.*$" nil t))

        (if (or (not end) (= begin 0))
            nil
          (progn
            (beginning-of-line)
            (setq end (point-marker))
            (setq begin (set-marker (make-marker) begin))

            (if insert-pager
                (progn
                  (goto-char begin)
                  (previous-line)
                  (end-of-line)
                  (insert (format "\n%s\n" rfc2org-page-delimiter))

                  (goto-char end)
                  (beginning-of-line)
                  (insert (format "%s\n\n" rfc2org-page-delimiter))))
            
            (goto-char begin)
            (while (re-search-forward
                    "^\\([ \t]*\\)\\([0-9]+\\(?:\\.[0-9]+\\)*\\)" end t)
              (replace-match "\\1[[\\2]]")
              (setq counter (1+ counter)))
            counter))))))


(defun target-buffer-name (buffer &optional newname)
  (let ((oldname (or (buffer-file-name buffer)
                     (buffer-name buffer))))
    (if newname
        (if (eq (length (file-name-extension newname)) 0)
            (setq newname (format "%s.org" (file-name-sans-extension oldname))))
      (setq newname (format "%s.org" (file-name-sans-extension oldname))))
    (file-name-nondirectory newname)))


(defun header-string (section)
  (let ((ret nil)
        nstar
        (seq (append section nil))
        (lev 1))
    (mapcar '(lambda (ch) (if (eq ch ?\.) (setq lev (1+ lev)))) seq)
    (setq nstar (- (* lev 2) 1))
    (while (> nstar 0)
      (setq ret (concat ret "*"))
      (setq nstar (1- nstar)))
    ret))


(defun insert-anchors (buffer)
  (set-buffer buffer)
  (let ((counter 0))
    (save-excursion
      (save-restriction

        (widen)
        (goto-char (point-min))

        (while (re-search-forward 
                "^\\([0-9]+\\(?:\\.[0-9]+\\)*\\)\\([ \t.]+.*\\)$" 
                nil t)
          (replace-match (format "%s \\1\\2 <<\\1>>" 
                                 (header-string (match-string 1))))
          (setq counter (1+ counter)))))
    counter))


(defun tokenize-tocline ()
  (let ((end (point-max)) pos)
    (setq pos (re-search-forward
               "^[ \t]+\\([0-9.]+\\)[ \t]*\\(.*\\)\.+\\([0-9]+\\)[ \t]*$"
               nil t))
    (if (and pos (<= pos end))
        (progn
          (goto-char pos)
          ;; (match-string 1) => "1.3"
          ;; (match-string 2) => "Terminology ..."
          ;; (match-string 3) => "8"
          ;(list (match-string 1) 
          ;(trim-string-right (match-string 2) " .") 
          ;(match-string 3)))
          (cons (match-string 1) (trim-string-right (match-string 2) " .")))
      nil)))


(provide 'rfc2org)
