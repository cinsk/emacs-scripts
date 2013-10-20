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

;; $Id: xskel.el,v 1.2 2010/12/16 08:26:16 cinsk Exp $

;;; Code:

(require 'skeleton)
(require 'autoinsert)
(require 'xlicense)

;;(eval-when-compile (require 'cl))

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
  "# -*-org-*-\n"
  "#+OPTIONS: toc:4" \n
  "#+STYLE: <style>body { "
  "font-family: \"Helvetica Neue\",Helvetica,Arial,Verdana,Geneva,sans-serif;"
  "}</style>\n"
  "#+STYLE: <style>body { font-size: 90%; }</style>\n"
  "#+STYLE: <style>pre.example { font-family: monospace; }</style>\n"
  "#+STYLE: <style>pre { font-family: monospace; }</style>\n"
  "#+LINK: google http://www.google.com/search?q=%s\n"
  "#+LINK: rfc http://www.rfc-editor.org/rfc/rfc%s.txt\n"
  "#+TODO: TODO(t) | DONE(d) CANCELED(c) POSTPONED\n"
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

(defun org-safe-skeleton ()
  "Call `org-skeleton' iff the buffer is created by the user"
  (when (and (buffer-file-name)
             (string-match "^[^* ]" (buffer-name)))
    (org-skeleton)))

(add-to-list 'auto-insert-alist
             '(("\\.h\\'" . "C header")
               . c-header-skeleton))

(add-to-list 'auto-insert-alist
             '(("\\.\\(H\\|hh\\|hpp\\|hxx\\)\\'" . "C++ header")
               . cxx-header-skeleton))

(add-to-list 'auto-insert-alist
             '(("\\.\\(html\\|htm\\|xhtml\\)\\'" . "HTML file")
               . xhtml-skeleton))

(add-to-list 'auto-insert-alist '("\\.org\\'" . org-safe-skeleton))

(add-to-list 'auto-insert-alist
             '(python-mode . py-init-skeleton))

(provide 'xskel)
;;; xskel.el ends here
