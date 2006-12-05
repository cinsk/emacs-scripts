;;; snippets.el --- Utility elisp functions

;;;
;;; Copyright (C) 2006  Seong-Kook Shin <cinsky@gmail.com>
;;;

;;;
;;; $Id$
;;;

(when nil
  ;; Not required for now.
  (eval-when-compile
    (require 'cl)))


(defmacro char (string index)
  "Return the INDEX-th caracter of STRING"
  `(aref ,string ,index))

(defmacro string-left-trim (seq string)
  "Like `string-trim', but only trims from the front."
  `(string-both-trim ,seq nil ,string))


(defmacro string-right-trim (seq string)
  "Like `string-trim', but only trims from the back."
  `(string-both-trim nil ,seq ,string))


(defun string-trim (seq string)
  "Remove any characters in SEQ from the beginning and the end of STRING."
  (string-both-trim seq seq string))


(defun string-both-trim (lseq rseq string)
  "Remove specified characters from the both ends of STRING.
First, remove any characters in LSEQ from the beginning of the STRING.
Then, remove any characters in RSEQ from the end of the STRING."
  (let ((l (length string)) (i 0) (j 0))
    (while (and (< i l)
                (find (char string i) lseq))
      (setq i (1+ i)))
    (setq j l)
    (while (and (< i j)
                (find (char string (1- j)) rseq))
      (setq j (1- j)))
    (if (and (= i 0) (= j l))
        string
      (substring string i j))))


(provide 'snippets)
