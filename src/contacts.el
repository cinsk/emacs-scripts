;;; contacts.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Seong-Kook Shin

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


(setq cinsk/contact-databases '(joyent-contacts))

(defun cinsk/update-contacts-database (decorator)
  (let ((database ()))
    (dolist (var cinsk/contact-databases)
      (let ((db (symbol-value var)))
        (dolist (rec db)
          (let ((key (car rec))
                (fname (cadr rec))
                (lname (caddr rec))
                (email (cadddr rec)))
            (setq database (cons (cons (concat fname " " lname ", <" email ">")
                                       (apply decorator (list fname lname email)))
                                 database))))))
    database))

(setq cinsk/contact-database (cinsk/update-contacts-database (lambda (f l e) (format "[[mailto:%s][%s %s]]" e f l))))

(setq cinsk/ido-completing-choices (mapcar (lambda (x) (car x)) cinsk/contact-database))

(defun cinsk/org-insert-contact ()
  (interactive)
  (let ((contact (ido-completing-read "mailto: " cinsk/ido-completing-choices)))
    (insert (cdr (assoc contact cinsk/contact-database)))))

(provide 'contacts)
;;; contacts.el ends here
