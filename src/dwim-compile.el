;;; dwim-compile.el --- context-aware compile command

;; Copyright (C) 2015  Seong-Kook Shin

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

;; dwim-compile provides context-aware compilation command.  Some
;; usages are:
;;
;; 1) If a source package is maintained one of maven, sbt, or gradle, it
;;    will automatically find the build tool file, then execute the build
;;    tool in that directory, not the directory of the current buffer.
;;
;; 2) Provide base template for the `compile' command.  For example,
;;    the compliation command will set to "cc" for `c-mode' and set to
;;    "c++" for `c++-mode'.
;;
;; Use `dwim-c/compile' instead of `compile' in your convenient key binding,
;; For example, add following snippets in your init file:
;;
;;    (require 'dwim-compile)
;;    (global-set-key [(control ?c) ?c] 'dwim-c/compile)
;;
;; `dwim-c/compile' will try to determine the possible build tools
;; described in `dwim-c/build-tool-alist'.  If multiple tools are possible,
;; it will ask you with completion.   Once a tool selected, it will cached
;; in the buffer local variable, `dwim-c/selected-build-tool'.  If you
;; invoke `dwim-c/compile' again, the cached result is called directly.
;;
;; If you want to ignore the cached result, provide a prefix argument
;; (C-u) to `dwim-c/compile'.  If you want to call `compile' directly,
;; provide two prefix arguments (C-u C-u).
;;
;;; Code:

(require 'cl-lib)

(defvar dwim-c/build-tool-alist
  '((sbt "\\`build\\.sbt\\'" nil t)
    (maven "\\`pom\\.xml\\'" nil t)
    (clojure clojure-mode "lein run")
    (gradle "\\`build\\.gradle\\'" gradle-execute t)
    (c c-mode "cc -Wall -c %s")
    (cc c++-mode "c++ -Wall -c %s")
    (make "\\`\\(Makefile\\|makefile\\|GNUmakefile\\)\\'" nil t)
    (latex latex-mode (TeX-command-master tex-compile))
    (tex tex-mode (TeX-command-master tex-compile))
    (emacs-lisp emacs-lisp-mode dwim-c/byte-compile))
  "Each entry has the form, (TOOL-NAME MATCH FUNC-OR-COMMANDS SEARCH-PARENTS)

TOOL-NAME is just a symbol represent the entry used in the
completion of build tools, just pick any reasonable name.

MATCH is a regular expression that represent the file name of the
specific tool.  Note that this regular expression need to match
whole file name.  Otherwise, `dwim-c/compile' will be confused.
The regular expression match will try to match file names in the
current directory, then try the parent directory until the root
directory.  You may pass major-mode name instead of a regular
expression.  In this case, only the current buffer mode is
matched.

If the argument, FUNC-OR-COMMANDS is nil, then `compile' will be
called in the directory that MATCH mached.  If it is non-nil,
then it must be one of (1) a command name to execute, (2) a
function with one argument, which is a file name of the current
buffer if any or the current buffer itself, or (3) a list of
commands or functions.  If it is a list, then the first available
one is used.

If the optional SEARCH-PARENTS is non-nil and MATCH is a regular
expression, then it will try to find the MATCHed file from the
current directory to the root directory.

Note that if FUNC-OR-COMMANDS is a command name, there is no way
to pass prefix arguments in it.")

(defvar dwim-c/selected-build-tool nil
  "Buffer local variable to cache the result of the selected build tool.")
(make-variable-buffer-local 'dwim-c/selected-build-tool)

(defvar dwim-c/keep-search-parents nil
  "If tool specific files are found in a certain directory,
dwim-compile will stop to find more tools in the parent directories.
If this variable is non-nil, then dwim-c will always try to find
tools in the parent directories.")

(defvar dwim-c/stop-at-first-build-tool nil
  "dwim-compile will try to find multiple build tools in three stages.
First, it will find the build tool by major mode match.  Second,
it will find build tools by regular expression matches without
traversing parent directories.  Third, it wil find build tools by
regular expression matches with traversing parent directories.

If this variable is non-nil, then dwim-compile will not proceed to the
next stage.")

(defvar dwim-c/disable-caching-selected nil
  "After a build tool is executed, dwim-compile will save the selection
in the buffer-local variable, `dwim-c/selected-build-tool' to cache the
finding build tools.  If you don't like this, set it to non-nil.")

(defvar dwim-c/build-tool-regexp "")
(defvar dwim-c/build-tool-regexp-np "")

(defun dwim-c/generate-build-tool-regexp (&optional search-parents)
  "Generate one big regular expression from `dwim-c/build-tool-alist'."
  (let ((re (concat "\\("
                    (mapconcat
                     (lambda (entry)
                       (if (symbolp (cadr entry)) "" (cadr entry)))
                     (cl-remove-if (lambda (ent)
                                  (or (symbolp (cadr ent))
                                      (and (stringp (cadr ent))
                                           (not (eq (not (not (nth 3 ent)))
                                                    search-parents)))))
                                dwim-c/build-tool-alist)
                     "\\|")
                    "\\)")))
    (if (string-equal re "\\(\\)")
        "$a"                   ; regular expression that never matches
      re)))


(defun dwim-c/setup ()
  "Set required variables from `dwim-c/build-tool-alist'."
  ;; This function must be called whenever `dwim-c/build-tool-alist'
  ;; is modified.  Currently, this function is called in
  ;; `dwim-c/comple', which it may sounds inefficient.
  (setq dwim-c/build-tool-regexp-np (dwim-c/generate-build-tool-regexp)
        dwim-c/build-tool-regexp (dwim-c/generate-build-tool-regexp t)))


(defun dwim-c/parent-directory (&optional directory)
  "Return the parent directory name of DIRECTORY.

If DIRECTORY is nil, `default-directory' will be used."
  (let ((dir (or directory default-directory)))
    (file-name-directory (directory-file-name (expand-file-name dir)))))

(defun dwim-c/root-directory-p (&optional directory)
  "Return non-nil if DIRECTORY is the root directory.

If DIRECTORY is nil, `default-directory' will be used."
  ;; Note that in Windows, the root directory will look like "c:/" not "/".
  (let ((dir (file-name-as-directory
              (expand-file-name (or directory default-directory)))))
    (string-equal dir (dwim-c/parent-directory dir))))


(defun dwim-c/assoc-sum (as1 as2)
  "Import entries in alist, AS2 if it not found in AS2.

Note that this function may modify AS1 destructively."
  (let (missing)
    (dolist (ent as2)
      (unless (assoc (car ent) as1)
        (add-to-list 'missing ent)))
    (append as1 missing)))

(defun dwim-c/detect-build-tools-from-cache (&optional nocache)
  "Return a list of build-tool specs from the cached result.

The cached spec was stored in `dwim-c/selected-build-tool'.  See also
`dwim-c/call-build-tool' for the format of the cached spec."
  (condition-case e
      (unless nocache
        (let* ((toolfile (cadr dwim-c/selected-build-tool))
               (basedir (file-name-directory toolfile))
               (pattern (concat "^" (regexp-quote basedir))))
          (when (and default-directory
                     (file-exists-p toolfile)
                     ;; Assume that the cached spec was set in a
                     ;; interactive buffer (e.g. "*shell*" buffer), and
                     ;; the user changed `default-directory' somehow
                     ;; unrelated directory of the spec.  In this case, we
                     ;; cannot use the cached spec, since the current
                     ;; directory is unrelated to the cached spec.
                     (string-match-p pattern default-directory))
            (list dwim-c/selected-build-tool))))
    (error nil)))


(defun dwim-c/detect-build-tools (&optional nocache)
  (let ((tools (dwim-c/detect-build-tools-from-cache nocache)))
    (when (not tools)
      (setq tools (dwim-c/assoc-sum tools (dwim-c/detect-build-tools-by-re-np)))
      (when (or (not dwim-c/stop-at-first-build-tool)
                (not tools))
        (setq tools (dwim-c/assoc-sum tools
                                      (dwim-c/detect-build-tools-by-re)))
        (when (or (not dwim-c/stop-at-first-build-tool)
                  (not tools))
          (setq tools (dwim-c/assoc-sum tools
                                        (dwim-c/detect-build-tool-by-mode))))))
    (remq nil tools)))

(defun dwim-c/detect-build-tools-by-re-np (&optional dir nocache)
  (let* ((basedir (expand-file-name (or dir default-directory)))
         (files (directory-files basedir nil dwim-c/build-tool-regexp-np))
         tools)
    (dolist (file files)
      (dolist (toolent (cl-remove-if
                        (lambda (ent)
                          (not (and (stringp (cadr ent))
                                    (null (nth 3 ent))
                                    (string-match (cadr ent) file))))
                        dwim-c/build-tool-alist))
        (setq tools
              (cons (list (car toolent)
                          (concat (file-name-as-directory basedir)
                                  file)) tools))))
    tools))

(defun dwim-c/detect-build-tools-by-re (&optional dir)
  "Find build tool entry from the current directory using
`dwim-c/build-tool-alist' by the regular expression in it.

Optional DIR is the starting directory.  If it is nil, it will"
  (let ((basedir (expand-file-name (or dir default-directory)))
        (continue? t)
        tools)
    (catch 'dwim-c/found
      (while continue?
        (let ((files (directory-files basedir nil dwim-c/build-tool-regexp)))
          (dolist (file files)
            (dolist (toolent (cl-remove-if
                              (lambda (ent)
                                (not (and (stringp (cadr ent))
                                          (string-match (cadr ent) file))))
                              dwim-c/build-tool-alist))
              (setq tools
                    (cons (list (car toolent)
                                (concat (file-name-as-directory basedir)
                                        file)) tools))))
          ;; In each directory, this function will try to find all
          ;; matched regular expression from
          ;; `dwim-c/build-tool-alist'.  Once it found any, it will
          ;; stop trying parent directories.
          (when (and files (not dwim-c/keep-search-parents))
            (throw 'dwim-c/found t))
          (if (dwim-c/root-directory-p basedir)
              (setq continue? nil)
            (setq basedir (dwim-c/parent-directory basedir))))))
    tools))

(defun dwim-c/detect-build-tool-by-mode ()
  "Find build tools from `dwim-c/build-tool-alist' by the major mode."
  (let (tools)
    (dolist (ent (delq nil (mapcar (lambda (ent)
                                     (if (symbolp (cadr ent)) ent))
                                   dwim-c/build-tool-alist)))
      (if (eq major-mode (cadr ent))
          (setq tools (cons (list (car ent) (or buffer-file-name
                                                (current-buffer)))
                            tools))))
    tools))

(defun dwim-c/compile-wrapper (spec &optional template)
  (let* ((cmdhist (caddr dwim-c/selected-build-tool))
         (compile-command
          (or cmdhist
              (if template
                  (format template
                          (if buffer-file-name
                              (file-name-nondirectory buffer-file-name)
                            ""))
                compile-command))))
    (call-interactively #'compile)
    (unless dwim-c/disable-caching-selected
      (setq dwim-c/selected-build-tool (list (car spec)
                                             (cadr spec)
                                             (car compile-history))))))


(defun dwim-c/call-build-tool (spec &optional arg)
  "Call the build tool according to the SPEC.

SPEC has the form, (TOOL-NAME FILENAME COMMAND-HIST) where TOOL-NAME is the
same symbol used in `dwim-c/build-tool-alist'. FILENAME is the
string of the full pathname of the build tool specific file such
as \"Makefile\".

This function will call FUNC-OR-COMMAND of the matched entry in
`dwim-c/build-tool-alist'.  See `dwim-c/build-tool-alist' for the
description of FUNC-OR-COMMAND."
  (let ((cmds (caddr (assoc (car spec) dwim-c/build-tool-alist)))
        (default-directory (file-name-directory (cadr spec)))
        (current-prefix-arg arg))
    (if cmds
        (if (stringp cmds)
            (dwim-c/compile-wrapper spec cmds)
          (let ((cmd (cl-find-if #'fboundp (if (listp cmds)
                                            cmds
                                          (list cmds)))))
            (if (commandp cmd)
                (progn (call-interactively cmd)
                       (unless dwim-c/disable-caching-selected
                         (setq dwim-c/selected-build-tool spec)))
              (if (functionp cmd)
                  (progn (funcall cmd (cadr spec))
                         (unless dwim-c/disable-caching-selected
                           (setq dwim-c/selected-build-tool spec)))
                (dwim-c/compile-wrapper spec)))))
      (dwim-c/compile-wrapper spec))))

(defun dwim-c/byte-compile (&optional buffer-or-file)
  "byte compile after prompting for byte compilation."
  (if (and (stringp buffer-or-file)
           (y-or-n-p (format "Byte compile file, %s?"
                             (file-name-nondirectory buffer-or-file))))
      (byte-compile-file buffer-or-file)
    (call-interactively #'byte-compile-file)))


(defun dwim-c/compile (&optional arg)
  "Issue compile command by the context.

It will call the appropriate function or the command from the
configuration in `dwim-c/build-tool-alist'.  With a prefix argument (C-u),
it will ignore any cached result and rebuild all possible candidates.
With two prefix arguments (C-u C-u), it will just call `compile' command."
  (interactive "p")
  (let ((nocache (if (eq arg 4) t))) ; nocache is t with `C-u'
    (dwim-c/setup)
    (if (>= arg 16)                     ; `C-u C-u'
        (progn
          (setq dwim-c/selected-build-tool nil)
          ;; Since the purpose of `C-u C-u' is to ignore dwim-compile,
          ;; it is intentional that call `compile' instead of
          ;; `dwim-c/compile-wrapper' here.
          (call-interactively #'compile))
      (let ((tools (dwim-c/detect-build-tools nocache)))
        (cond ((null tools)
               (call-interactively #'compile))
              ((= 1 (length tools))
               (dwim-c/call-build-tool (car tools)))
              ((> (length tools) 1)
               (let ((spec (assoc (intern (completing-read "Build tool: "
                                                           tools nil t
                                                           nil ; initial
                                                           nil ; hist
                                                           nil ; default
                                                           )) tools)))
                 (dwim-c/call-build-tool spec))))))))

(defun string/starts-with (s begins)
  "Return non-nil if string S starts with BEGINS."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))

(defun compilation-finished-notification (buf status)
  (cond ((string/starts-with status "finished")
         (shell-command "say -v Vicki done"))
        (t
         (shell-command "say -v Agnes oops"))))

(add-hook 'compilation-finish-functions 'compilation-finished-notification)

(provide 'dwim-compile)
;;; dwim-compile.el ends here
