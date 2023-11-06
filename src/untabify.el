;;; untabify.el ---                                  -*- lexical-binding: t; -*-

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

;;
;; TAB & space setting
;;
(setq-default indent-tabs-mode nil)     ; do not insert tab character.


;; These hook configuration ensures that all tab characters in C, C++
;; source files are automatically converted to spaces on saving.
(defvar untabify-remove-trailing-spaces-on-write-modes
  '(c-mode c++-mode java-mode emacs-lisp-mode lisp-mode nxml-mode)
  "List of major mode that needs to convert tab characters into spaces,
and to remove trailing whitespaces")

(defun untabify-remove-trailing-spaces-on-write ()
  "Utility function that removes tabs and trailing whitespaces"
  (when (memq major-mode untabify-remove-trailing-spaces-on-write-modes)
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace))
  ;; Should return nil so that if this function is registered into
  ;; `write-contents-functions', and if we need to propagate the control
  ;; to the other functions in `write-contents-functions'.
  ;;
  ;; Personally, this function should be registered into
  ;; `before-save-hook' anyway.
  nil)


(provide 'untabify)
;;; untabify.el ends here
