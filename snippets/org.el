;; -*-emacs-lisp-*-

;;;
;;; Org mode
;;;
(eval-after-load "org"
  '(progn
     (define-key outline-mode-map [(control down)]
       'outline-next-visible-heading)
     (define-key outline-mode-map [(control up)]
       'outline-previous-visible-heading)
     (define-key outline-mode-map [(control shift down)]
       'outline-forward-same-level)
     (define-key outline-mode-map [(control shift up)]
       'outline-backward-same-level)

     ;; Rebind `org-force-cycle-archived' from "C-<TAB>" to "C-x C-<TAB>"
     ;; since I use "C-<TAB>" for `smart-other-window'.
     (move-key org-mode-map [(control tab)] [(control x) (control tab)])

     ;; Move the binding of `org-deadline' from "C-c C-d" to "C-c
     ;; C-S-d", since I'vd used the keybinding for
     ;; `delete-chars-forward-with-syntax'.
     (move-key org-mode-map [(control ?c) (control ?d)]
               [(control ?c) (control shift ?d)])

     (define-key org-mode-map [(control c) (control ?\\)]
       'org-table-convert-from-lines)

     (define-key org-mode-map [(control c) ?t] 'org-todo)
     ;; When opening a link with `org-open-at-point' (C-c C-o), These
     ;; settings allow to use acroread for pdf files and to use ggv
     ;; for ps files.
     (add-to-list 'org-file-apps '("pdf" . "acroread %s"))
     (add-to-list 'org-file-apps '("ps" . "ggv %s"))))

(when (locate-library "org-version")
  ;; From Org-Mode version 7.9.2, there is no need to load
  ;; 'org-install.  Worse, it will display a warning, which makes me
  ;; uncomfortable.  Following sexp will load 'org-install if the
  ;; installed version is lower than 7.9.2.
  (require 'org-version)
  (if (and (fboundp 'org-release)
           (not (string-lessp (org-release) "7.9.2")))
      nil                               ; do nothing
    (require 'org-install)))

;; According to http://orgmode.org/org.html#Conflicts, filladapt.el is
;; not working well with Org, so disable it in org-mode.
(add-hook 'org-mode-hook '(lambda ()
                            (if (fboundp 'turn-off-filladapt-mode)
                                (turn-off-filladapt-mode))))

;; Org mode requires font-locking on every org buffer
;; Since I use global-font-lock-mode, below sexp is not necessary.
;;
;; (add-hook 'org-mode-hook 'turn-on-font-lock)

;; org-hide-leading-stars should be set before loading org-mode.
(setq org-hide-leading-stars t)
(setq org-odd-levels-only t)
(setq org-agenda-include-diary t)

;; If org file loaded with folding, comparing files with ediff
;; is very unhandy, thus starting with everything is shown
(setq org-hide-block-startup nil)
(setq org-startup-folded 'showeverything)

(when (string-lessp emacs-version "22.2")
  ;; From GNU Emacs version 22.2, ".org" extension use Org mode by default.
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)))

(global-set-key [(control c) ?a] 'org-agenda)
(global-set-key [(control c) ?l] 'org-store-link)
(global-set-key [(control c) ?b] 'org-iswitchb)
(global-set-key [(control c) ?\"] 'org-capture)

(org-remember-insinuate)
(global-set-key [f8] 'org-capture)

(let* ((org-path (getenv "ORG_PATH"))
       (my-org-directory (if org-path
                             org-path
                           (concat (file-name-as-directory
                                    user-emacs-directory)
                                   "agenda"))))
  ;; All of my org agena files are located in `my-org-directory'.
  (if (not (file-accessible-directory-p my-org-directory))
      (if (yes-or-no-p
           (format "create org directory(%s)? " my-org-directory))
          (make-directory my-org-directory t)))

  (if (file-accessible-directory-p my-org-directory)
      (let ((notefile (concat (file-name-as-directory my-org-directory)
                              "notes.org")))
        ;; Install all .org files in `my-org-directory' if exists
        (setq org-agenda-files
              (directory-files my-org-directory t ".*\\.org\\'"))
        (setq org-default-notes-file notefile)
        (setq org-directory my-org-directory))
    (lwarn '(dot-emacs) :warning
           (format "cannot access org files in %s." my-org-directory))))

(setq org-capture-templates
      '(("w" "Work-related TODO" entry
         (file+headline (concat (file-name-as-directory org-directory)
                                "work.org")
                        "Tasks")
         "* TODO %? %T\n  %i\n  %a")
        ("p" "Personal Project-related TODO" entry
         (file+headline (concat (file-name-as-directory org-directory)
                                "pproject.org")
                        "Tasks")
         "* TODO %? %T\n  %i\n  %a")
        ("P" "Personal TODO" entry
         (file+headline (concat (file-name-as-directory org-directory)
                                "personal.org")
                        "Tasks")
         "* TODO %? %T\n  %i\n  %a")
        ("j" "Personal Journal (date based)" entry
         (file+datetree (concat (file-name-as-directory org-directory)
                                "personal.org"))
         "* %?\n  Entered on %U\n  %i\n  %a")))

;; (add-to-list 'org-agenda-files "~/.emacs.d/personal.org")

(defvar org-table-convert-last-nrows    3
  "Default number of columns per row.  This is changed if user used
another value")

(defun org-table-convert-from-lines (&optional nrows)
  "Convert lines to the org table. Each line contains one column
so that users need to specify the number of columns per row.

For example, if the region contains 9 lines and each line
contains the digit from 1 to 9, calling
`org-table-convert-from-lines' with the column number 3 makes the
following:

| 1 | 2 | 3 |
| 4 | 5 | 6 |
| 7 | 8 | 9 |"
  (interactive "P")
  (require 'org)
  (if (null nrows)
      (let ((nrows (string-to-number
                    (read-string
                     (format "Number of columns per row[%d]: "
                             org-table-convert-last-nrows)
                     nil nil
                     (number-to-string org-table-convert-last-nrows)))))
        (setq org-table-convert-last-nrows nrows)
        (save-excursion
          (save-restriction
            (let ((start (set-marker (make-marker) (region-beginning)))
                  (end (set-marker (make-marker) (region-end))))
              ;;(message "nrows(%S) start(%S) end(%S)" nrows start end)
              (set-marker-insertion-type end t)
              (narrow-to-region start end)
              (goto-char start)
              (while (progn
                       (dotimes (i (1- nrows))
                         (end-of-line) (zap-to-nonspace) (insert "\t"))
                       (beginning-of-line)
                       (and (eq (forward-line) 0) (< (point) end))))
              (org-table-convert-region start end '(16))))))))

(setq org-export-html-style "
<link rel=\"stylesheet\" type=\"text/css\"
      href=\"http://www.cinsk.org/bootstrap/bootstrap.css\"/>
<link rel=\"stylesheet\" type=\"text/css\"
      href=\"http://www.cinsk.org/bootstrap/bootstrap-responsive.css\"/>
<link rel=\"stylesheet\" type=\"text/css\"
      href=\"http://www.cinsk.org/bootstrap/docs.css\"/>
<style type=\"text/css\">
  <!--/*--><![CDATA[/*><!--*/
  body { margin: 2% 6% 2% 10%; }
  h2 { margin-left: -7%; }
  h3 { margin-left: -5%; }
  pre  { background-color: Black; color: White; }
  /*]]>*/-->
</style>
")

;;
;; yasnippet support (tested with yasnippets 0.8.0 beta)
;;
;; See also http://orgmode.org/org.html#Conflicts
;;
(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (and (fboundp 'yas-expand) (yas-expand))))

(add-hook 'org-mode-hook
          (lambda ()
            ;; (make-variable-buffer-local 'yas/trigger-key)
            ;; (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook
                         'yas-org-very-safe-expand)
            ;; (define-key yas/keymap [tab] 'yas/next-field)
            ))




(when (and (> emacs-major-version 22)
           (fboundp 'org-set-emph-re))
  ;; Current org-mode mark-up algorithm does not support marking
  ;; partial word. (e.g. =partial=word)
  ;;
  ;; On some languages that have postposition, which has no word
  ;; boundary with the previous noun (e.g. Korean aka josa),
  ;; marking-up partial word is essential.
  ;;
  ;; To work around current implementation, it is possible to insert
  ;; invisible unicode character such as "word joiner" character,
  ;; \u2060, between the noun and the postposition, to enable partial
  ;; word.  Thus the text will be "=partial=\u2060word", and adding
  ;; this special character to `org-emphasis-regexp-components' will
  ;; do the trick. (Use `ucs-insert' to insert the character into the
  ;; text)
  ;;
  ;; See the original idea from:
  ;;   http://thread.gmane.org/gmane.emacs.orgmode/46197/focus=46263
  (org-set-emph-re 'org-emphasis-regexp-components
                   '(" \t('\"{"
                     "- \t.,:!?;'\")}\\\u2060"
                     " \t\r\n,\"'⁠"
                     "." 1)))

(define-key org-mode-map [(control c) (control ?\;)] '(lambda ()
                                                        (interactive)
                                                        (ucs-insert #x2060)))


(defvar cinsk/mode-name-list
  (let (lst)
    (mapatoms (lambda (sym)
                (let ((name (symbol-name sym)))
                  (if (string-match "^\\([^-]*\\)-mode$" name)
                      (push (match-string 1 name) lst)))))
    lst)
  "names of the Emacs mode that can be used in SRC block in org-mode")

(defvar cinsk/org-sourcefy-history nil
  "history variable for `org-sourcefy'.")

(setq cinsk/org-sourcefy-history nil)

(defun cinsk/org-sourcefy (mode begin end)
  "Enclose the current region with #+BEGIN_SRC ... #+END_SRC"
  (interactive
   (list
    (let ((initial (and (boundp 'cinsk/org-sourcefy-history)
                    (car cinsk/org-sourcefy-history))))
      ;; don't know why, but HIST arg in `completing-read' is not working
      (completing-read "mode: " cinsk/mode-name-list nil t initial
                       'cinsk/org-sourcefy-history))
    (region-beginning)
    (region-end)))

  (let ((text (buffer-substring-no-properties begin end))
        require-newline-at-end)
    (goto-char end)
    (if (not (bolp))
        (setq require-newline-at-end t))

    (delete-region begin end)
    (goto-char begin)
    (push-mark)
    (insert (if (bolp) "" "\n")
            (format "#+BEGIN_SRC %s\n" mode)
            text
            (if require-newline-at-end "\n" "")
            "#+END_SRC\n")
    (push mode cinsk/org-sourcefy-history)))

(define-key org-mode-map [(control c) ?s] 'cinsk/org-sourcefy)
