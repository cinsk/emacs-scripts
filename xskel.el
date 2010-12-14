;;; xskel.el --- Insert a pre-defined license text

;; Copyright (C) 2010  Seong-Kook Shin

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

;; $Id$

;;; Code:

(require 'skeleton)
(require 'autoinsert)

(eval-when-compile (require 'cl))

(defvar license-directory "~/.emacs.d/license"
  "Directory for license templates")

(defvar license-types '((gpl . "GPL-2.0")
                        (gpl2 . "GPL-2.0")
                        (gpl3 . "GPL-3.0")
                        (lgpl . "LGPL")
                        (apache . "APACHE-2.0")
                        (boost . "BOOST")
                        (bsd-old . "BSD-old")
                        (bsd-new . "BSD-new")
                        (bsd . "BSD-new")
                        (freebsd . "FREEBSD")
                        (mit . "MIT")
                        (tmp . "TMP")
                        )
  "Alist of licenses.  CAR of each item is a symbol represents the license,
CDR of each item is a filename of the license template")

(defvar license-default-summary
  "<one line to give the program's name and a brief idea of what it does.>"
  "Short description of what it does.")

(defvar license-eol-text "!@#$EOL!@#$"
  "Text to mark blank lines -- used internally")

(defun license-file (type)
  "Return the pathname of the given license file"
  (let ((tp (assoc type license-types)))
    (if tp
        (concat (file-name-as-directory license-directory) (cdr tp))
      tp)))

(defvar xskel-keywords-alist '(("@author@" . user-full-name)
                               ("@email@" . "hello")
                               ("@year@" . (lambda ()
                                             (substring (current-time-string)
                                                        -4)))
                               ("@organization@" . (lambda ()
                                                     (getenv "ORGANIZATION"))))
  "Keywords that need to be substituted by `xskel-substitute-keywords'.

The CAR of an item is a keyword and CDR is a replacement.  If the
CDR of an item is a function, the return value(string) is used as
a replacement.  If the returned value is nil, no substitution for
that keyword.")


(defun xskel-substitute-keywords (&optional record)
  (let (markers)
    (dolist (i xskel-keywords-alist)
      (let ((keyword (regexp-quote (car i)))
            (what (if (functionp (cdr i)) (funcall (cdr i)) (cdr i))))
        (if what
            (progn
              (goto-char (point-min))
              (while (re-search-forward keyword nil t)
                (if record
                    (setq markers (cons (point-marker) markers)))
                (replace-match what))))))
    markers))

(defun xskel-fill-paragraphs (lst)
  (dolist (i lst)
    (goto-char i)
    (fill-paragraph)))


(defun create-license (type &optional summary author)
  (let (;(buffer (get-buffer-create "*LICENSE*"))
        (desc (or (and summary (> (length summary) 0))
                  license-default-summary))
        (auth (or author (user-full-name)))
        (lfile (license-file type))
        (mode major-mode)
        (fill-points nil))
    ;;(save-current-buffer
    (with-temp-buffer
      ;(set-buffer buffer)
      ;(erase-buffer)
      (insert (format "%s\n" desc))
      (insert (format "Copyright (C) %d  %s" (nth 5 (decode-time)) auth))
      (if user-mail-address
          (insert (format " <%s>" user-mail-address)))
      (insert "\n")
      (insert-file-contents lfile)

      (goto-char (point-min))
      (while (re-search-forward "^$" nil t)
        (replace-match license-eol-text))

      (let ((case-fold-search t)
            (markers (xskel-substitute-keywords t)))
        (funcall mode)
        (let ((comment-style 'extra-line))
          (comment-region (point-min) (point-max)))

        (goto-char (point-min))

        (let ((re-eol (concat (regexp-quote license-eol-text) "$")))
          (while (re-search-forward re-eol nil t)
            (replace-match "")))

        (xskel-fill-paragraphs markers)

        ;;(print markers)
        ;;(pop-marker)
        (goto-char (point-max))
        (insert "\n"))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun create-license-old (type &optional summary author)
  (let (;(buffer (get-buffer-create "*LICENSE*"))
        (desc (or (and summary (> (length summary) 0))
                  license-default-summary))
        (auth (or author (user-full-name)))
        (lfile (license-file type))
        (mode major-mode)
        (fill-points nil))
    ;;(save-current-buffer
    (with-temp-buffer
      ;(set-buffer buffer)
      ;(erase-buffer)
      (insert (format "%s\n" desc))
      (insert (format "Copyright (C) %d  %s" (nth 5 (decode-time)) auth))
      (if user-mail-address
          (insert (format " <%s>" user-mail-address)))
      (insert "\n")
      (insert-file-contents lfile)

      (goto-char (point-min))
      (while (re-search-forward "^$" nil t)
        (replace-match license-eol-text))

      (let ((case-fold-search t)
            (markers nil))
        (flet ((push-marker () 
                            (setq markers (cons (point-marker) markers)))
               (pop-marker ()
                           (if (null markers)
                               nil
                             (let ((m (car markers)))
                               (goto-char m)
                               (setq markers (cdr markers))
                               m))))
          (goto-char (point-min))
          (while (re-search-forward "<author>" nil t)
            ;;(message (format "push mark at %d" (match-beginning 0)))
            (push-marker)
            (replace-match (user-full-name)))

          (goto-char (point-min))
          (let ((org (getenv "ORGANIZATION")))
            (if org
                (while (re-search-forward "<organization>" nil t)
                  ;;(message (format "push mark at %d" (match-beginning 0)))
                  (push-marker)
                  (replace-match org))))

          (funcall mode)
          (let ((comment-style 'extra-line))
            (comment-region (point-min) (point-max)))

          (goto-char (point-min))

          (let ((re-eol (concat (regexp-quote license-eol-text) "$")))
            (while (re-search-forward re-eol nil t)
              (replace-match "")))

          (while (pop-marker)
            (fill-paragraph))

          ;;(print markers)
          ;;(pop-marker)
          (goto-char (point-max))
          (insert "\n")
          ))
      (buffer-substring-no-properties (point-min) (point-max)))))


(defun insert-license (&optional type)
  (interactive)
  (let ((text (create-license
               (or type (intern (completing-read "Choose a license type: "
                                                 license-types nil t))))))
    (if (called-interactively-p 'any)
        (insert text)
      text)))


(defun c-file-name-macro ()
  "Return a string in XXX_H__ form where XXX is the upcase name of the file"
  (upcase (concat (file-name-nondirectory
                   (file-name-sans-extension buffer-file-name))
                  "_"
                  (file-name-extension buffer-file-name) "__")))


(define-skeleton c-header-skeleton
  "cinsk's personal skeleton for a blank C header file"
  (c-file-name-macro)
  (insert-license)
  "#ifndef " str \n
  "#define " str "\n\n"
  "/* This indirect using of extern \"C\" { ... } makes Emacs happy */" \n
  "#ifndef BEGIN_C_DECLS" \n
  "# ifdef __cplusplus" \n
  "#  define BEGIN_C_DECLS extern \"C\" {" \n
  "#  define END_C_DECLS   }" \n
  "# else" \n
  "#  define BEGIN_C_DECLS" \n
  "#  define END_C_DECLS" \n
  "# endif" \n
  "#endif /* BEGIN_C_DECLS */" \n
  "\n\n"
  _ "\n\n#endif /* " str " */")

(define-skeleton cxx-header-skeleton
  "cinsk's personal skeleton for a blank C++ header file"
  (c-file-name-macro)
  "/* -*-c++-*- */" \n
  (insert-license) \n
  "#ifndef " str \n
  "#define " str "\n\n"
  "#ifndef __cplusplus" \n
  "#error This is a C++ header file" \n
  "#endif"
  "\n\n"
  "#ifndef BEGIN_NAMESPACE" \n
  "#define BEGIN_NAMESPACE(x)      namespace x {" \n
  "#define END_NAMESPACE(x)        }" \n
  "#endif"
  "\n\n"
  _ "\n\n#endif /* " str " */")

(define-skeleton gpl-interactive-skeleton
  "Insert an Interactive GPL banner."
  ""
  \n > "static const char *gpl_banner[] = {" \n
  > "\"" (file-name-nondirectory (file-name-sans-extension buffer-file-name))
  > " version XXX, Copyright (C) "
  (substring (current-time-string) -4) " " (user-full-name) "\"," \n
  > "\"" (file-name-nondirectory (file-name-sans-extension buffer-file-name))
  "comes with ABSOLUTELY NO WARRANTY; for details type `show w'.\"," \n
  "\"This is free software, and you are welcome to redistribute it\"," \n
  "\"under certain conditions; type `show c' for details.\"," \n
  > > "};" \n
  > _)

(define-skeleton xhtml-skeleton
  "Insert an XHTML skeleton into the current buffer"
  nil
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?> <!-- -*-nxml-*- -->\n"
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n"
  "          \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
  "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"ko\" lang=\"ko\">\n"
  "  <head>\n"
  "    <meta http-equiv=\"Content-Type\" content=\"text/html; "
  "charset=utf-8\"/>\n"
  "    <title></title>\n"
  "    <!-- <link rel=\"shortcut icon\" href=\"\"/> -->\n"
  "    <!-- <link rel=\"stylesheet\" type=\"text/css\"\n"
  "               href=\"\" title=\"default\"></link> -->\n"
  "  </head>\n\n"
  "  <body>\n"
  > _
  "\n"
  "  </body>\n"
  "</html>\n"
  "<!--\n"
  " Local Variables:\n"
  " fill-column: 78\n"
  "-->\n"
)

(define-skeleton py-init-skeleton
  "doc"
  ""
  "#!/usr/bin/env python" \n
  "# -*-python-*-" \n \n
  "\"\"\"A documentation string of this module." \n
  "\"\"\"" \n \n
  _
  \n \n
  "if __name__ == \"__main__\":" \n
  > "main()" \n)

(define-skeleton org-skeleton
  "Inserts a ORG mode skeleton into current buffer."
  ""
  "# -*-org-mode-*-\n"
  "#+OPTIONS: toc:4" \n
  "#+STYLE: <style>body { "
  "font-family: \"Helvetica Neue\",Helvetica,Arial,Verdana,Geneva,sans-serif;"
  "}</style>\n"
  "#+STYLE: <style>body { font-size: 90%; }</style>\n"
  "#+STYLE: <style>pre.example { font-family: monospace; }</style>\n"
  "#+STYLE: <style>pre { font-family: monospace; }</style>\n"
  "#+LINK: google http://www.google.com/search?q=%s\n"
  "#+LINK: rfc http://www.rfc-editor.org/rfc/rfc%s.txt\n"
  "#+DRAWERS: PROPERTIES CLOCK LOGBOOK COMMENT\n"
  "#+TITLE: " (skeleton-read "Agenda Title: " (buffer-name)) \n
  "#+AUTHOR: " (skeleton-read "Author: " (user-full-name)) \n
  "#+EMAIL: " (skeleton-read "email: " user-mail-address) \n
  "\n"
  ":COMMENT:\n"
  "# Markup hints:\n"
  "#   *bold*, /italic/, _underlined_, =code=, ~verbatim~\n"
  "#   Use '\\\\' at the end of a line to force a line break.\n"
  "#   Use \"[[URL or TARGET][NAME]]\" to create a hyperlink.\n"
  "#   Use \"[[google:KEYWORD][DESC]]\" to link to Google with KEYWORD.\n"
  "#   Use \"[[rfc:NUMBER][DESC]]\" to link to RFC-NUMBER.txt.\n"
  "#   `C-c C-o' to follow a link target.\n"
  "#   Use \"#+BEGIN_VERSE ... #+END_VERBSE\" to format poetry\n"
  "#   Use \"#+BEGIN_QUOTE ... #+END_QUOTE\" to format a quotation.\n"
  "#   Use \"#+BEGIN_CENTER ... #+END_CENTER\" to center some text.\n"
  "#   `C-c C-x f' for footnote action(jump or insert).\n"
  "#   Unordered list items start with `-', `+', or `*' as bulllets.\n"
  "#   Ordered list items start with  `1.' or `1)'.\n"
  ":END:\n"
  "\n" _)

(add-to-list 'auto-insert-alist 
             '(("\\.h\\'" . "C header")
               . c-header-skeleton))

(add-to-list 'auto-insert-alist
             '(("\\.\\(H\\|hh\\|hpp\\|hxx\\)\\'" . "C++ header")
               . cxx-header-skeleton))
             
(add-to-list 'auto-insert-alist
             '(("\\.\\(html\\|htm\\|xhtml\\)\\'" . "HTML file")
               . xhtml-skeleton))

(add-to-list 'auto-insert-alist '("\\.org\\'" . org-skeleton))

(add-to-list 'auto-insert-alist
             '(python-mode . py-init-skeleton))

(provide 'xskel)
;;; xskel.el ends here
