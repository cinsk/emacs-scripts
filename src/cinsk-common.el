;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; cinsk-common.el

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

(require 'cl-lib)

(unless (fboundp 'with-eval-after-load)
  ;; with-eval-after-load was added in Emacs 24.4
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature."
    (declare (indent 1) (debug t))
    `(eval-after-load ,file (lambda () ,@body))))


(defun cinsk/priortize-auto-mode (mode)
  "Sort(prepend) `auto-mode-alist' entries that has MODE in them."
  (setq auto-mode-alist
        (let (newlst filtered)
          (if (rassoc mode auto-mode-alist)
              (progn
                (dolist (pair auto-mode-alist)
                  (if (eq (cdr pair) mode)
                      (setq filtered (cons pair filtered))
                    (setq newlst (cons pair newlst))))
                (append filtered (nreverse newlst)))
            auto-mode-alist))))


(defun cinsk/add-site-lisp-directories (site-dir)
  "Add the subdirectories of SITE-DIR into `load-path'.

This function will ignore any subdirectory of SITE-DIR does not
contains .el file."
  (let ((basedir site-dir))
    ;; Add subdirectories of "~/.emacs.d/local/" iff it has any .el file.
    (when (file-accessible-directory-p basedir)
      (dolist (dir (cl-remove-if-not
                    (lambda (dir)
                      (let ((files (file-expand-wildcards
                                    (file-name-concat dir "*.el"))))
                        (> (length files) 0)))
                    (directory-files basedir 'full-name "^[^.]")))
        (add-to-list 'load-path dir)))))

(defun cinsk/accessible-directories (&rest dirs)
  "Return the list of directories that are accessible.

Each elements in DIRS will be expanded using `expand-file-name'."
  (remove nil (mapcar (lambda (path)
                        (setq path (expand-file-name path))
                        (if (file-accessible-directory-p path)
                            path))
                      dirs)))

;; ;;; I haven't tested throughly, but macro version of `move-key' makes
;; ;;; `cider-jack-in' causes problems.
;; (defmacro move-key (key-map old-bind new-bind)
;;   "Move the bound command from OLD-BIND to NEW-BIND.
;;
;; This macro returns the command of NEW-BIND if any, otherwise
;; returns nil.  If there is no command for OLD-BIND, this macro
;; will do nothing."
;;   (let ((new-key (gensym))
;;         (old-key (gensym)))
;;     `(let ((,new-key (lookup-key ,key-map ,new-bind))
;;            (,old-key (lookup-key ,key-map ,old-bind)))
;;        (when ,old-key
;;          (keymap-set ,key-map ,new-bind ,old-key)
;;          (keymap-set ,key-map ,old-bind nil)
;;          ,new-key))))

(defun cinsk/key-string (key)
  "Return KEY as a string accepted by `keymap-set'.

KEY may be a string already in `key-valid-p' form (\"C-c C-k\"), or
a raw key sequence: a vector ([(control ?k)]) or the string result
of `kbd'."
  (if (and (stringp key) (key-valid-p key))
      key
    (key-description key)))

(defun cinsk/move-key (keymap old-key new-key)
  "Move the key definition from OLD-KEY to NEW-KEY in KEYMAP.

OLD-KEY and NEW-KEY are anything `cinsk/key-string' accepts.
It will return the previous key definition bound in NEW-KEY, and
the definition bound in OLD-KEY will be removed."
  (let* ((old-key (cinsk/key-string old-key))
         (new-key (cinsk/key-string new-key))
         (def (keymap-lookup keymap old-key))
         (alt (keymap-lookup keymap new-key)))
    (keymap-set keymap new-key def)
    (keymap-set keymap old-key nil)
    alt))

(defun cinsk/copy-key (keymap src-key dst-key)
  "Rebind the command currently bound to SRC-KEY to DST-KEY."
  (let* ((src-key (cinsk/key-string src-key))
         (def (keymap-lookup keymap src-key)))
    (keymap-set keymap (cinsk/key-string dst-key) def)))


(cl-defun my/move-key (&key from to with keymap overwrite)
  "Move the binding of key sequence FROM to key sequence TO, then bind FROM to WITH.

FROM and TO are key sequences: strings in `kbd' syntax (\"C-x f\")
or vectors ([?\\C-x ?f]).  The definition of FROM can be a command,
a keymap (a prefix key) or any other valid binding.  It is copied
to TO as it is.

WITH is the new definition for FROM.  If it is nil, FROM becomes
unbound in KEYMAP.

KEYMAP is the keymap to work in.  It defaults to the current
global map.  Only KEYMAP itself is looked at.  Bindings that come
from minor modes or parent keymaps are not considered.

If TO already has a different binding in KEYMAP, signal an error
unless OVERWRITE is non-nil.

Return the definition that was moved.

Example:
  (move-key :from \"C-x f\" :to \"C-x F\" :with #\\='find-file
            :keymap my-mode-map)"
  (unless (and from to)
    (error "move-key: both :from and :to are required"))
  (let* ((map      (or keymap (current-global-map)))
         (from-key (if (stringp from) from (key-description from)))
         (to-key   (if (stringp to)   to   (key-description to)))
         (old-def  (lookup-key map (key-parse from-key)))
         (existing (lookup-key map (key-parse to-key))))
    ;; A numeric result from `lookup-key' means a prefix of the
    ;; sequence is bound to something that is not a keymap.
    (when (or (null old-def) (numberp old-def))
      (user-error "move-key: %s has no binding in this keymap"
                  from-key))
    (when (and existing
               (not (numberp existing))
               (not (equal existing old-def))
               (not overwrite))
      (user-error "move-key: %s is already bound to %S (use :overwrite t)"
                  to-key existing))
    (keymap-set map to-key old-def)
    (keymap-set map from-key with)
    old-def))


(defun cinsk/visit-tags-table (file &optional local)
  "Call `visit-tags-table' iff FILE is readable"
  (and (file-readable-p file)
       (visit-tags-table file local)))

(defun cinsk/source-untabify ()
  "Stealed from Jamie Zawinski's homepage,
http://www.jwz.org/doc/tabs-vs-spaces.html
Remove any right trailing whitespaces and convert any tab
character to the spaces"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
        (untabify (1- (point)) (point-max))))
  nil)


(defun cinsk/import-buffer-region (&optional after-import)
  "Copy region from the current buffer to the previous buffer.

Once called, Emacs enters in recursive edit mode.  Marking a region
in some buffer then press \\[exit-recursive-edit] will copy the region
into the buffer at the invocation time.

If the function AFTER-IMPORT is non-nil, this function will call
AFTER-IMPORT with the buffer where the user press
\\[exit-recursive-edit].  In the AFTER-IMPORT, the mark is set to
the beginning of the inserted text, and the point is set to the
end of the inserted text.

This function uses `recursive-edit' internally."
  (interactive)
  (let* ((map (current-global-map))
         (old-binding (lookup-key map [(control meta ?c)])))
    (substitute-key-definition
     'exit-recursive-edit
     'cinsk/exit-import-buffer-region map)
    ;; (keymap-set map "C-M-c" #'cinsk/exit-import-buffer-region)

    (let ((old-buffer (current-buffer))
          (src-buffer (unwind-protect
                          (catch 'exit-from-import
                            (message "Use `%s' when done, or use `%s' to abort."
                                     (substitute-command-keys "\\[cinsk/exit-import-buffer-region]")
                                     (substitute-command-keys "\\[abort-recursive-edit]"))
                            (recursive-edit))
                        ;; (keymap-set map "C-M-c" old-binding))))
                        (substitute-key-definition
                         'cinsk/exit-import-buffer-region
                         'exit-recursive-edit
                         map))))
      (when (buffer-live-p old-buffer)
        (let ((display-buffer-reuse-frames t)
              start end)
          (pop-to-buffer old-buffer)
          (with-current-buffer src-buffer
            (when (and mark-active
                       (or (and transient-mark-mode
                                (use-region-p))
                           (not transient-mark-mode)))
              (setq start (region-beginning)
                    end (region-end))))
          (when (and start end)
            (push-mark)
            (insert-buffer-substring src-buffer start end)
            (and after-import
                 (funcall after-import src-buffer))
            (pop-mark)))))))

(defun cinsk/exit-import-buffer-region ()
  (interactive)
  (throw 'cinsk/exit-from-import (current-buffer)))

(defun cinsk/line-numbers-on-region (begin end &optional start)
  "Insert line numbers on the region.

When called interactively, it insert the line number starting
from 1, in the region.  A numeric prefix argument specifies the
starting number."
  (interactive (list
                (region-beginning)
                (region-end)
                (if current-prefix-arg
                    (prefix-numeric-value current-prefix-arg)
                  1)))
  (unless start (setq start 1))
  (let ((begin (region-beginning))
        (end (region-end)))
    (save-restriction
      (let* ((lines (count-lines begin end))
             (width (length (format "%d" (1- (+ lines start)))))
             (fmt (format "%%%dd: " width)))
        (goto-char begin)
        (beginning-of-line)
        (dotimes (i lines)
          (insert (format fmt (+ start i)))
          (forward-line))))))


(defun cinsk/fill-text-line-paragraph (begin end)
  "Convert each line in the region to a filled-paragraph"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)

      (let ((begin (set-marker (make-marker) begin))
            (end (set-marker (make-marker) end)))
        (set-marker-insertion-type end t)
        (beginning-of-line)
        (goto-char begin)
        (while (eq (forward-line) 0)
          (newline))

        (goto-char begin)
        (while (progn
                 (fill-paragraph nil)
                 (eq (forward-line) 0)))))))

(defun cinsk/unfill-paragraph (&optional arg)
  (interactive "P")
  (let ((fill-column (point-max)))
    (fill-paragraph arg t)))



(defmacro with-popup-buffer-window (&rest body)
  "Pop up new window at bottom and evaulate BODY.

This function will create a window at bottom, showing temporary
empty buffer.  The buffer will be the current buffer while excuting
BODY.   Then it will ask the user to press <RET> to destroy the window
and the buffer, returns the last expression of the BODY."
  (declare (indent 0) (debug t))
  `(with-current-buffer-window (generate-new-buffer " *popup*")
       '(display-buffer-at-bottom . ((dedicated . t)
                                     (window-height . fit-window-to-buffer)))
       (lambda (w v)
         (with-selected-window w
           (unwind-protect
               (progn (read-from-minibuffer "Press <RET> to continue...")
                      v)
             (when (window-live-p w)
               (quit-restore-window w 'kill)))))
     ,@body))


;;
;; Launch view-mode when visiting other's file.
;;
(defun cinsk/file-uid (filename)
  (cl-caddr (file-attributes (expand-file-name filename))))

(defun cinsk/smart-view-mode ()
  "Enable `view-mode' on certain condition.

If the user is not the owner of the file, or if the file's truename is
not belong to user's home directory, then enable `view-mode'.

This function is best used in `find-file-hook'."
  (let* ((file (and buffer-file-name (file-truename buffer-file-name)))
         (fuid (and file (cinsk/file-uid file)))
         (home (getenv "HOME")))
    (setq home (and home (file-truename home)))
    (when (or (and (not (null fuid))          ; file exists,
                   (not (eq fuid (user-uid))) ; uid/owner differs,
                   (not (eq (user-uid) 0)))   ; not root,
              (and home file
                   ;; if the file does not belong to user's $HOME,
                   (not (string-equal home (substring file 0 (length home))))))
      (view-mode 1))))

(add-hook 'find-file-hook 'cinsk/smart-view-mode)

(provide 'cinsk-common)
;;; cinsk-common.el ends here
