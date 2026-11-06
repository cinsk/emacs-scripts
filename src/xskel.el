;;; xskel.el --- Personal auto-insert skeletons  -*- lexical-binding: t; -*-

;; Copyright (C) 2010  Seong-Kook Shin

;; Author: Seong-Kook Shin <cinsky@gmail.com>
;; Keywords: convenience, tools

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

;; Skeletons for new files (C/C++ headers, HTML, Python, Org), registered
;; in `auto-insert-alist'.
;;
;; Auto-insertion only happens in file buffers that were opened by one of
;; the interactive commands listed in `xskel-auto-insert-commands'.  Files
;; that packages create or visit programmatically (org-capture, drafts,
;; history files, ...) are therefore left alone.

;;; Code:

(require 'skeleton)
(require 'autoinsert)
(require 'xlicense)


;;; Helpers

(defun c-file-name-macro ()
  "Return an include-guard macro name for the current file.
For \"foo-bar.h\" this returns \"FOO_BAR_H_\".  Every character that is
not valid in a C identifier is replaced by an underscore."
  (concat (upcase (replace-regexp-in-string
                   "[^A-Za-z0-9]" "_"
                   (file-name-nondirectory buffer-file-name)))
          "_"))

(defun xskel-maybe-insert-license ()
  "Call `insert-license', treating C-g as \"no license\"."
  (condition-case nil
      (insert-license)
    (quit nil)))

;;; Skeletons

(define-skeleton c-header-skeleton
  "cinsk's personal skeleton for a blank C header file"
  (c-file-name-macro)
  (xskel-maybe-insert-license)
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
  (xskel-maybe-insert-license)
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
  "-->\n")

(define-skeleton py-init-skeleton
  "Insert a skeleton for a new Python script."
  nil
  "#!/usr/bin/env python3" \n
  "# -*-python-*-" \n \n
  "\"\"\"A documentation string of this module." \n
  "\"\"\"" \n \n
  _
  \n \n
  "if __name__ == \"__main__\":" \n
  > "main()" \n)

(defun my/org-html-theme-url ()
  "Return the location of setup file for org html export"
  (let ((path (file-name-concat user-emacs-directory "extern/html-theme-readtheorg.setup")))
    (if (file-readable-p path)
	path
      "https://raw.githubusercontent.com/fniessen/org-html-themes/refs/heads/master/org/setup/html-theme-readtheorg.setup")))

(define-skeleton org-skeleton
  "Inserts a ORG mode skeleton into current buffer."
  nil
  "# -*-org-*-\n"
  "#+options: d:(not \"LOGBOOK\" \"COMMENT\")\n"
  "#+link: google http://www.google.com/search?q=%s\n"
  "#+link: rfc http://www.rfc-editor.org/rfc/rfc%s.txt\n"
  "#+todo: TODO(t) | DONE(d) CANCELED(c) POSTPONED\n"
  "#+latex_class: article\n"
  "#+latex_class_options: [letterpaper]\n"
  "#+latex_header: \\usepackage{kotex}\n"
  "#+title: " (or (file-name-base) "") \n
  "#+author: " (or (user-full-name) "") \n
  "#+email: " (or user-mail-address "") \n
  "#+setupfile: " (my/org-html-theme-url) \n
  "{{{disable-search}}}\n"
  "\n"
  "#+begin_comment\n"
  "Markup hints:\n"
  "  - *bold*, /italic/, _underlined_, =code=, ~verbatim~\n"
  "  - Use '\\\\' at the end of a line to force a line break.\n"
  "#+begin_example\n"
  "  ,#+name: my target        # to create internal TARGET.\n"
  "  - <<target>>another item, we refer to item [[target]]\n"
  "  [[URL or TARGET][NAME]]   # to create a hyperlink.\n"
  "  [[google:KEYWORD][DESC]]  # to link to Google with KEYWORD.\n"
  "  [[rfc:NUMBER][DESC]]      # to link to RFC-NUMBER.txt.\n"
  "  [[man:printf(3)][DESC]]          # to link to man printf(3).\n"
  "  [[java:java.lang.String][DESC]]  # to link to Java API\n"
  "#+end_example\n"
  "  - `C-c C-l' to insert a hyperlink.\n"
  "  - `C-c C-o' to follow a link target.\n"
  "  - Use \"#+begin_verse ... #+end_verse\" to format poetry\n"
  "  - Use \"#+begin_quote ... #+end_quote\" to format a quotation.\n"
  "  - Use \"#+begin_center ... #+end_center\" to center some text.\n"
  "  - `C-c C-x f' for footnote action(jump or insert).\n"
  "  - Unordered list items start with `-', `+', or `*' as bullets.\n"
  "  - Ordered list items start with  `1.' or `1)'.\n"
  "\n"
  "  For the equation, use \\(...\\) for inlined one, or \\[...\\] for the\n"
  "  display math.  Use \\begin{align}...\\end{align} for multi-line\n"
  "  equation with '&' for alignment mark.  For example:\n"
  "#+begin_src org\n"
  "  \\begin{align}\n"
  "    x & = r \\cos \\theta \\\\\n"
  "    y & = r \\sin \\theta \\\\\n"
  "  \\end{align}\n"
  "#+end_src\n"
  "\n"
  "  To numbering use \\begin{XXX}...\\end{XXX} instead \\[...\\].\n"
  "\n"
  "#+begin_src org\n"
  "  \\begin{equation}\n"
  "  E = mc^2 \\label{emc}\n"
  "  \\end{equation}\n"
  "  See Equation $\\ref{emc}$ for more.\n"
  "#+end_src\n"
  "\n"
  "  - See [[http://en.wikibooks.org/wiki/LaTeX/Mathematics][Math]] or [[http://cdn.mathjax.org/mathjax/latest/test/sample-eqnum.html][sample-eqnum]] for math equations.\n"
  "\n"
  "  - You can specify the source language and optionally put the header\n"
  "    how Org handles the result of the execution, either =value= or\n"
  "    =output=.  You can execute/insert the result by pressing =C-c C-c=.\n"
  "    See also [[info:org#Evaluating Code Blocks][info \"(org) Evaluating Code Blocks\"]]\n"
  "\n"
  "#+begin_src bash :results output\n"
  "echo \"hello, world\"\n"
  "#+end_src\n"
  "\n"
  "Table hints:\n"
  "  - =C-c C-c= to re-align the table\n"
  "  - Use \"<N>\" in the field to specify the width of the column.\n"
  "  - Use =C-c `= to edit such a field.\n"
  "  - See also [[info:org#Column groups][info \"(org) Column groups\"]]\n"
  "  - See also [[info:org#The spreadsheet][info \"(org) The spreadsheet\"]]\n"
  "#+end_comment\n"
  "\n"
  _)


;;; Registration

;; `add-to-list' (rather than `define-auto-insert') is deliberate: it is
;; idempotent, so re-evaluating this file does not stack up duplicate
;; actions for the same condition.  New entries go to the front, so they
;; take precedence over the stock entries.

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

;; Match on the file name, not on `python-mode': `python-ts-mode' does
;; not derive from `python-mode', so a mode-based entry would miss it.
(add-to-list 'auto-insert-alist '("\\.py\\'" . py-init-skeleton))


;;; Restrict auto-insert to files opened interactively

(defcustom xskel-auto-insert-commands
  '(auto-insert                         ; so M-x auto-insert still works
    find-file
    find-file-other-window
    find-file-other-frame
    find-file-other-tab
    find-file-at-point
    ido-find-file
    counsel-find-file
    helm-find-files)
  "Commands after which auto-insert is allowed to run.
`auto-insert' compares against `this-command', so a file visited by a
package from inside some other command (e.g. `org-capture') is skipped."
  :type '(repeat symbol)
  :group 'auto-insert)

(define-advice auto-insert (:around (orig &rest args) xskel-filter)
  "Only auto-insert in file buffers opened by an interactive file command."
  (when (and buffer-file-name
             (memq this-command xskel-auto-insert-commands))
    (apply orig args)))

(auto-insert-mode 1)


(provide 'xskel)
;;; xskel.el ends here
