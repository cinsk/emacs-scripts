;;-*-emacs-lisp-*-
;;-*-coding: emacs-mule;-*-

;; $Id$

;;
;; Each Item has a form `(ABBREVNAME EXPANSION [HOOK] [PROPS...])'.
;;
(define-abbrev-table 'global-abbrev-table
  '(
    ("$$license" "" license-skeleton)
    ))

(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("$$igpl" "" gpl-interactive-skeleton)
    ("$$niy" "// TODO: Not Implemented Yet." nil 0)
    ))

(define-abbrev-table 'c-mode-abbrev-table
  '(
    ("$$igpl" "" gpl-interactive-skeleton)
    ("$$niy" "/* TODO: Not Implemented Yet. */" nil 0)
    ))

(define-abbrev-table 'nxml-mode-abbrev-table
  '(
    ("$$doctypexhtml" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
	\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" nil 0)
    ("$doctypexhtml" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
	\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" nil 0)
    ))

