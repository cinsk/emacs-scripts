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


(defun trim-string-right (string &optional trail)
  "Return a copy of STRING with all trailing characters removed.
Optional TRAIL specifies the trailing characters in a string form.
If omitted, whitespace characters are used.  For example:

\(trim-string-right \"abcd  \")      => \"abcd\"
\(trim-string-right \"abc.. \" \" .\") => \"abc\"

See also `trim-string-left'."
  (let* ((len (length string))
         (i (1- len))
         (trail (or trail " \t\n\v")))
    (setq trail (append trail nil))
    (while (and (>= i 0)
                (member (elt string i) trail))
      (setq i (1- i)))
    (if (eq i (1- len))
        string
      (substring string 0 (1+ i)))))


(defun trim-string-left (string &optional leading)
  "Return a copy of STRING with all leading characters removed.
Optional LEADING specifies the leading characters in a string form.
If omitted, whitespace characters are used.  For example:

\(trim-string-left \"  abcd\")      => \"abcd\"
\(trim-string-left \" .abcd\" \" .\") => \"abcd\"

See also `trim-string-right'."
  (let ((len (length string))
        (i 0)
        (leading (or leading " \t\n\v")))
    (setq leading (append leading nil))
    (while (and (< i len)
                (member (elt string i) leading))
      (setq i (1+ i)))
    (substring string i len)))


(provide 'snippets)
