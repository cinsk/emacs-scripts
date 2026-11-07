;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;
;;; Org mode
;;;
;;; Org itself is loaded lazily: everything that needs Org's own
;;; definitions (keymaps, customizations with :set functions, ...)
;;; lives in the `with-eval-after-load' form below.  Plain variable
;;; settings are done at top level so they are already in place
;;; when Org (or org-agenda / org-capture) is first loaded.

(require 'seq)
(require 'subr-x)

(defvar org-mode-map)
(defvar org-src-lang-modes)
(defvar org-file-apps)
(declare-function org-table-align "org-table")
(declare-function org-insert-structure-template "org")
(declare-function org-in-src-block-p "org")
(declare-function org-edit-src-code "org-src")
(declare-function org-edit-src-exit "org-src")


;;;; Basic settings

(setq org-adapt-indentation t           ; indent to outline node level
      org-hide-leading-stars t
      org-odd-levels-only t
      ;; If an org file is loaded folded, comparing files with ediff
      ;; is very unhandy, so start with everything shown.
      org-hide-block-startup nil
      org-startup-folded 'showeverything
      ;; Don't ask before evaluating code blocks.
      org-confirm-babel-evaluate nil)


;;;; Link abbreviations and descriptions

(setq org-link-abbrev-alist
      '(("google" . "https://www.google.com/search?q=%s")
        ("rfc"    . "https://www.rfc-editor.org/rfc/rfc%s.txt")
        ("man"    . "%(my/org-link-man-url)")
        ;; ("java" . "%(my/org-link-java-url)")
        ))

;; Org refuses to call a function from a link abbreviation unless
;; the function is marked as safe.
(put 'my/org-link-man-url 'org-link-abbrev-safe t)
(put 'my/org-link-java-url 'org-link-abbrev-safe t)

(defun my/org-link-man-url (spec)
  "Return a man7.org URL for the manual page described by SPEC.

SPEC is \"NAME(SECTION)\", \"SECTION NAME\", or just \"NAME\" (which
means section 1).  SECTION does not need to be numeric only, e.g.
\"scanf(3p)\" is fine.

Used by the \"man\" entry of `org-link-abbrev-alist', so that
[[man:printf(3)]] becomes a link to the online manual page."
  (pcase-let
      ((`(,name ,sect)
        (pcase (string-trim spec)
          ((rx bos (let n (+ (not (any space "(")))) (* space)
               "(" (* space) (let s (+ (not (any space ")")))) (* space) ")" eos)
           (list n s))
          ((rx bos (let s (+ (not space))) (+ space) (let n (+ (not space))) eos)
           (list n s))
          ((rx bos (let n (+ (not space))) eos)
           (list n "1"))
          (_ (user-error "Invalid manual page spec: %s" spec)))))
    ;; man7.org keeps e.g. "printf.3p" under the "man3" directory.
    (format "https://man7.org/linux/man-pages/man%s/%s.%s.html"
            (if (string-match "\\`[0-9]+" sect) (match-string 0 sect) sect)
            name sect)))

(defvar my/org-javadoc-root-alist
  '(("org.springframework.boot." . "https://docs.spring.io/spring-boot/docs/current/api/")
    ("org.springframework."      . "https://docs.spring.io/spring/docs/current/javadoc-api/")
    ("org.apache.log4j."         . "https://www.slf4j.org/apidocs/")
    ("org.slf4j."                . "https://www.slf4j.org/apidocs/")
    ("org.junit."                . "https://junit.org/junit4/javadoc/latest/")
    ("lombok."                   . "https://projectlombok.org/api/")
    ("java."                     . "https://docs.oracle.com/javase/8/docs/api/"))
  "Alist of (PACKAGE-PREFIX . BASE-URL) used by `my/org-link-java-url'.

Unlike other alists, the order matters: the first entry whose
prefix matches wins, so list more specific prefixes first.")

(defun my/org-link-java-url (class)
  "Return the Javadoc URL of the fully qualified CLASS name.
For example, \"org.slf4j.Logger\".  See `my/org-javadoc-root-alist'."
  (when-let ((entry (seq-find (lambda (ent) (string-prefix-p (car ent) class))
                              my/org-javadoc-root-alist)))
    (concat (cdr entry) (string-replace "." "/" class) ".html")))

(defvar my/org-link-description-alist
  '(("google" . nil)
    ("rfc"    . my/org-link-rfc-description)
    ("java"   . my/org-link-java-description))
  "Alist of (LINK-TYPE . FUNCTION) for `my/org-link-make-description'.

When a link \"TYPE:PATH\" is inserted without a description and
TYPE is listed here, FUNCTION is called with PATH and its return
value is used as the description.  If FUNCTION is nil or returns
nil, PATH itself is used.")

(defun my/org-link-make-description (link desc)
  "Default description for LINK; see `my/org-link-description-alist'.
DESC, if non-empty, is returned unchanged."
  (cond
   ((and desc (not (string-empty-p desc))) desc)
   ((string-match "\\`\\([^:]+\\):\\(.+\\)\\'" link)
    (let ((type (match-string 1 link))
          (path (match-string 2 link)))
      (if-let ((entry (assoc type my/org-link-description-alist)))
          (or (and (functionp (cdr entry)) (funcall (cdr entry) path))
              path)
        desc)))
   (t desc)))

(defun my/org-link-rfc-description (path)
  "Return \"RFC N\" for the rfc link PATH (e.g. \"0434\" or \"23\")."
  (let ((n (string-to-number path)))
    (unless (> n 0)
      (user-error "Invalid RFC number: %s" path))
    (format "RFC %s" path)))

(defun my/org-link-java-description (path)
  "Return the unqualified class name of the java link PATH."
  (car (last (split-string path "\\." t))))

(setq org-link-make-description-function #'my/org-link-make-description)


;;;; Org directory, agenda files and capture

(defvar my/org-directory nil
  "Directory for storing org files.

`my/org-init-directory' decides `org-directory' as follows:
the ORG_PATH environment variable if set, then this variable if
non-nil, and finally \"~/agenda\".

Why not just set `org-directory'?  It cannot express \"unset\":
Org gives it a default of \"~/org\" once loaded, so a fallback
chain reading it could never tell your choice from the default,
and `my/org-init-directory' overwrites it anyway.  This variable
is nil unless you set it, so the chain can tell the difference.
Set it before this file is loaded (e.g. in early init or a
machine-specific file); setting it later has no effect on
`org-agenda-files' or the capture templates.")

(defvar my/org-default-files nil
  "Org files that are created (empty) in `org-directory' if missing.")

(defvar my/org-agenda-exclude-regexps
  '(;; Dropbox conflicted copies, e.g.
    ;;   "sample (XXX's conflicted copy 2009-10-15).org"
    ;;   "sample (XXX와(과) 충돌하는 사본 2013-02-22).org"
    "conflicted copy [0-9-:]*)\\.org\\'"
    "충돌하는 사본 [0-9-:]*)\\.org\\'"
    ;; Syncthing conflicts
    "\\.sync-conflict-.*\\.org\\'")
  "Regexps matched against the file name (without directory).
Org files whose name matches any of them are not agenda files.")

(defvar my/org-agenda-exclude-tags '("noagenda" "ARCHIVE")
  "File tags (case-insensitive) that exclude an org file from the agenda.
An org file is excluded if its #+FILETAGS contain any of them.")

(defun my/org-file-tags (file)
  "Return the list of tags in the #+FILETAGS keyword(s) of FILE.
Multiple #+FILETAGS lines are combined, as Org does."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((case-fold-search t)
          tags)
      (while (re-search-forward "^[ \t]*#\\+FILETAGS:[ \t]*\\(.*\\)$" nil t)
        (setq tags (nconc tags (split-string (match-string-no-properties 1)
                                             "[: \t]+" t))))
      tags)))

(defun my/org-agenda-file-p (file &optional exclude-regexps exclude-tags)
  "Return non-nil if FILE should be one of `org-agenda-files'.

FILE is rejected if its non-directory name matches any regexp in
EXCLUDE-REGEXPS, or if its #+FILETAGS contain any tag in
EXCLUDE-TAGS (compared case-insensitively).  They default to
`my/org-agenda-exclude-regexps' and `my/org-agenda-exclude-tags'."
  (let ((name (file-name-nondirectory file))
        (regexps (or exclude-regexps my/org-agenda-exclude-regexps))
        (tags (or exclude-tags my/org-agenda-exclude-tags)))
    (and (file-readable-p file)
         (not (seq-some (lambda (re) (string-match-p re name)) regexps))
         (not (seq-intersection (my/org-file-tags file) tags
                                #'string-equal-ignore-case)))))

(defun my/org-agenda-files (&optional directory exclude-regexps exclude-tags)
  "Return the agenda-worthy .org files in DIRECTORY as absolute names.

DIRECTORY defaults to `org-directory'.  Hidden files and lock or
auto-save files (starting with \".\" or \"#\") are ignored, and so
is every file rejected by `my/org-agenda-file-p' with
EXCLUDE-REGEXPS and EXCLUDE-TAGS."
  (let ((dir (expand-file-name (or directory org-directory))))
    (when (file-accessible-directory-p dir)
      (seq-filter (lambda (f)
                    (and (file-regular-p f)
                         (my/org-agenda-file-p f exclude-regexps exclude-tags)))
                  (directory-files dir t "\\`[^.#].*\\.org\\'")))))

(defun my/org-agenda-refresh-files (&optional directory)
  "Set `org-agenda-files' to `my/org-agenda-files' of DIRECTORY.
Interactively, DIRECTORY is `org-directory'; with a prefix
argument, ask for it."
  (interactive
   (list (and current-prefix-arg
              (read-directory-name "Org directory: " org-directory nil t))))
  (setq org-agenda-files (my/org-agenda-files directory))
  (when (called-interactively-p 'interactive)
    (message "%d agenda files" (length org-agenda-files)))
  org-agenda-files)

(defun my/org-init-directory ()
  "Set up `org-directory', default files and `org-agenda-files'."
  (let ((dir (file-name-as-directory
              (expand-file-name (or (getenv "ORG_PATH")
                                    my/org-directory
                                    "~/agenda")))))
    (make-directory dir t)
    (if (not (file-accessible-directory-p dir))
        (lwarn 'dot-emacs :warning "cannot access org files in %s" dir)
      (dolist (f my/org-default-files)
        (let ((file (expand-file-name f dir)))
          (unless (file-exists-p file)
            (make-empty-file file)))))
    (setq org-directory dir
          org-default-notes-file (expand-file-name "notes.org" dir))
    (my/org-agenda-refresh-files dir)))

(my/org-init-directory)

(setq org-capture-templates
      `(("w" "Work-related TODO" entry
         (file+headline ,(expand-file-name "work.org" org-directory) "Tasks")
         "* TODO %? %T\n  %i\n  %a")
        ("f" "Finance Journal" entry
         (file+headline ,(expand-file-name "fjournal.org" org-directory) "Journal")
         "* %?%t\n  %i\n")
        ("p" "Personal Project-related TODO" entry
         (file+headline ,(expand-file-name "pproject.org" org-directory) "Tasks")
         "* TODO %? %T\n  %i\n  %a")
        ;; ("P" "Personal TODO" entry
        ;;  (file+headline ,(expand-file-name "personal.org" org-directory) "Tasks")
        ;;  "* TODO %? %T\n  %i\n  %a")
        ("e" "Emacs Tips" entry
         (file+headline ,(expand-file-name "emacs-tips.org" user-emacs-directory) "Tips")
         "* TODO %? %T\n  %i\n  %a")
        ("j" "Personal Journal (date based)" entry
         (file+olp+datetree ,(expand-file-name "personal.org" org-directory))
         "* %?\n  Entered on %U\n  %i\n  %a")
        ;; Diary entry in the date tree of ~/agenda/diary.org.  Asks for
        ;; the date (e.g. "fri", "+2w 3pm", "12/24"); a time of day, if
        ;; given, goes into the timestamp.  From the agenda (`k') or the
        ;; calendar (`i d'), the date at point is used without asking.
        ;; `my/org-capture-diary-timestamp' is defined in _calendar.el.
        ("d" "Diary entry (date-based)" entry
         (file+olp+datetree ,(expand-file-name "diary.org" org-directory))
         "* %? %(my/org-capture-diary-timestamp)\n  %i"
         :time-prompt t)))


;;;; HTML export

(setq org-html-head "
<link rel=\"stylesheet\" type=\"text/css\"
      href=\"https://cinsk.github.io/bootstrap/css/bootstrap.css\"/>
<link rel=\"stylesheet\" type=\"text/css\"
      href=\"https://cinsk.github.io/bootstrap/css/bootstrap-responsive.css\"/>
<link rel=\"stylesheet\" type=\"text/css\"
      href=\"https://cinsk.github.io/bootstrap/css/docs.css\"/>
<style type=\"text/css\">
  body { margin: 2% 6% 2% 10%; }
  h2 { margin-left: -7%; }
  h3 { margin-left: -5%; }
  pre  { background-color: Black; color: White; }
</style>
")


;;;; Commands

(defvar my/org-table-from-lines-columns 3
  "Last number of columns used by `my/org-table-from-lines'.")

(defun my/org-table-from-lines (beg end ncols)
  "Convert the lines between BEG and END to an org table of NCOLS columns.

Each non-blank line becomes one cell, filling the table row by
row.  For example, with NCOLS 3, nine lines containing the digits
1 to 9 become:

| 1 | 2 | 3 |
| 4 | 5 | 6 |
| 7 | 8 | 9 |

Interactively, NCOLS is the numeric prefix argument, or read from
the minibuffer (defaulting to the last value used)."
  (interactive
   (let ((n (if current-prefix-arg
                (prefix-numeric-value current-prefix-arg)
              (read-number "Number of columns per row: "
                           my/org-table-from-lines-columns))))
     (unless (use-region-p)
       (user-error "No active region"))
     (list (region-beginning) (region-end) n)))
  (unless (> ncols 0)
    (user-error "Number of columns must be positive"))
  (setq my/org-table-from-lines-columns ncols)
  ;; Operate on whole lines; a region ending at the beginning of a
  ;; line does not include that line.
  (let* ((beg (save-excursion (goto-char beg) (line-beginning-position)))
         (end (save-excursion
                (goto-char end)
                (when (and (bolp) (> (point) beg)) (backward-char))
                (line-end-position)))
         (cells (mapcar (lambda (s) (string-replace "|" "\\vert{}" s))
                        (split-string (buffer-substring-no-properties beg end)
                                      "\n" t "[ \t]+"))))
    (delete-region beg end)
    (goto-char beg)
    (insert (mapconcat (lambda (row) (concat "| " (string-join row " | ") " |"))
                       (seq-split cells ncols)
                       "\n"))
    (goto-char beg)
    (org-table-align)))

(defvar my/org-sourcefy-history nil
  "History of languages for `my/org-sourcefy'.")

(defun my/org-src-languages ()
  "Return candidate languages for an org source block."
  (let (langs)
    (mapatoms (lambda (sym)
                (let ((name (symbol-name sym)))
                  (when (and (string-suffix-p "-mode" name)
                             (commandp sym))
                    (push (string-remove-suffix "-mode" name) langs)))))
    (seq-uniq (append (mapcar #'car org-src-lang-modes) langs))))

(defun my/org-sourcefy (lang)
  "Wrap the region in a source block of LANG (\"#+begin_src LANG\").
Without an active region, insert an empty source block.  This is
`org-insert-structure-template' with completion for LANG; the
block contents are then re-indented by a round trip through
`org-edit-special'."
  (interactive
   (let ((def (car my/org-sourcefy-history)))
     (list (completing-read (format-prompt "Language" def)
                            (my/org-src-languages) nil 'confirm nil
                            'my/org-sourcefy-history def))))
  (let ((region (use-region-p)))
    (org-insert-structure-template (concat "src " lang))
    (when (and region (org-in-src-block-p))
      ;; Same as C-c ' C-c ', normalizing the block indentation.
      (save-excursion
        (when (org-edit-src-code)
          (org-edit-src-exit))))))

(defun my/org-insert-zero-width-space ()
  "Insert U+200B ZERO WIDTH SPACE, Org's escape character for mark-up.

Org mark-up cannot emphasize part of a word (e.g. =partial=word),
which matters for languages whose postpositions have no word
boundary with the preceding noun (e.g. Korean josa).  Org accepts
a zero width space as the boundary, so =partial=<ZWSP>word works,
both for display and export.  It also helps with text that starts
or ends with a quote: =<ZWSP>\"hello\"<ZWSP>=.
See Info node `(org) Escape Character'."
  (interactive)
  (insert-char #x200B))


;;;; Word joiner minor mode

(defconst my/org-wordjoin-regexp
  "\\([=/]\\)\\(.*\\)\\1\\([^[:space:]\u200B]+\\)"
  "Regexp matching an emphasized phrase followed by a partial word.
Group 1 is the marker, group 2 the phrase, group 3 the partial word.")

(defvar-keymap my/org-wordjoin-mode-map
  "SPC"   #'my/org-wordjoin-dwim
  "RET"   #'my/org-wordjoin-dwim
  "C-j"   #'my/org-wordjoin-dwim
  "TAB"   #'my/org-wordjoin-dwim
  "<tab>" #'my/org-wordjoin-dwim)

(define-minor-mode my/org-wordjoin-mode
  "Insert zero width spaces automatically after partial-word emphasis.
See `my/org-insert-zero-width-space'."
  :lighter " WordJoin"
  :keymap my/org-wordjoin-mode-map)

(defun my/org-wordjoin-dwim ()
  "Insert a ZWSP after a just-closed emphasis, then run the key's command.

If the text before point looks like =phrase=word, insert a zero
width space before \"word\" so the emphasis still works.  Then run the
command this key would run without `my/org-wordjoin-mode'."
  (interactive)
  (save-match-data
    (when (looking-back my/org-wordjoin-regexp (line-beginning-position) t)
      (save-excursion
        (goto-char (match-beginning 3))
        (insert-char #x200B))))
  (let ((cmd (let ((my/org-wordjoin-mode nil))
               (key-binding (this-command-keys-vector) t))))
    (when (commandp cmd)
      (setq this-command cmd)
      (call-interactively cmd))))

;; (add-hook 'org-mode-hook #'my/org-wordjoin-mode)


;;;; Global key bindings

(keymap-global-set "C-c a" #'org-agenda)
;; `q' in the agenda restores the windows as they were before the
;; agenda was shown (e.g. the calendar at the bottom, see `d' in the
;; calendar), instead of leaving the agenda's window layout behind.
(setq org-agenda-restore-windows-after-quit t)
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c b" #'org-switchb)
(keymap-global-set "C-c \"" #'org-capture)
(keymap-global-set "<f8>" #'org-capture)


;;;; After Org is loaded

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     ;; (R . t)
     (python . t)
     ;; (ruby . t)
     (dot . t)
     (shell . t)))

  (keymap-set outline-mode-map "C-<down>"   #'outline-next-visible-heading)
  (keymap-set outline-mode-map "C-<up>"     #'outline-previous-visible-heading)
  (keymap-set outline-mode-map "C-S-<down>" #'outline-forward-same-level)
  (keymap-set outline-mode-map "C-S-<up>"   #'outline-backward-same-level)

  ;; Rebind `org-force-cycle-archived' from "C-<tab>" to "C-x C-<tab>"
  ;; since I use "C-<tab>" for `smart-other-window'.
  ;; (my/move-key :keymap org-mode-map :from "C-<tab>" :to "C-x C-<tab>")

  ;; Move `org-deadline' from "C-c C-d" to "C-c C-S-d", since I use
  ;; "C-c C-d" for `delete-chars-forward-with-syntax'.
  (when (eq (keymap-lookup org-mode-map "C-c C-d") 'org-deadline)
    (my/move-key :keymap org-mode-map :from "C-c C-d" :to "C-c C-S-d"))

  ;; org-mode uses "C-c \"" as a prefix; use "C-c \" \"" for capture.
  (keymap-set org-mode-map "C-c \" \"" #'org-capture)
  (keymap-set org-mode-map "C-c C-\\" #'my/org-table-from-lines)
  (keymap-set org-mode-map "C-c t"    #'org-todo)
  (keymap-set org-mode-map "C-c S"    #'my/org-sourcefy)
  ;; Shift + TAB in rdesktop, xmanager, ...
  (keymap-set org-mode-map "S-<kp-tab>" #'org-shifttab)
  ;; Both keys insert Org's escape character (U+200B).  "C-c C-;"
  ;; used to insert U+2060 WORD JOINER, which no longer works for
  ;; export since Org 9.6 hard-codes the emphasis boundaries.
  (keymap-set org-mode-map "C-c C-;" #'my/org-insert-zero-width-space)
  (keymap-set org-mode-map "C-c C-'" #'my/org-insert-zero-width-space)

  (when (fboundp 'graphviz-dot-mode)
    (setf (alist-get "dot" org-src-lang-modes nil nil #'equal) 'graphviz-dot))

  ;; "Export as HTML and open" (C-c C-e h o) uses `org-open-file',
  ;; which may pick lynx(1) from ~/.mailcap.  Use `browse-url' instead.
  (add-to-list 'org-file-apps
               '("\\.x?html?\\'" . (lambda (file _link) (browse-url file))))
  (when (eq system-type 'gnu/linux)
    (add-to-list 'org-file-apps '("\\.pdf\\'" . "xdg-open %s"))
    (add-to-list 'org-file-apps '("\\.ps\\'" . "xdg-open %s")))

  ;; filladapt does not play well with Org.
  (when (fboundp 'filladapt-mode)
    (add-hook 'org-mode-hook (lambda () (filladapt-mode -1)))))

;; This setting causes org export to HTML to fail.
;;
;; (setq org-todo-keywords
;;       '((sequence "TODO" "|" "DONE")
;;         (sequence "RESEARCH" "|" "DEVELOP" "|" "TEST" "|" "OP-DONE")
;;         (sequence "IMPL" "|" "REVIEW" "|" "RELEASE" "|" "SP-DONE")))


;; When a org file first opened, expand all nodes except drawers and comment blocks.
(defun my/org-hide-comment-blocks ()
  "Fold all #+BEGIN_COMMENT ... #+END_COMMENT blocks."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^[ \t]*#\\+begin_comment\\b" nil t)
        (beginning-of-line)
        (if (fboundp 'org-fold-hide-block-toggle)   ; Org 9.6+
            (org-fold-hide-block-toggle t)
          (org-hide-block-toggle t))                ; older Org
        (forward-line 1)))))

(defun my/org-startup-visibility ()
  "Expand all headings; fold drawers and comment blocks."
  (org-show-all '(headings blocks))        ; expand everything except drawers
  (if (fboundp 'org-fold-hide-drawer-all)  ; fold :PROPERTIES:, :LOGBOOK:, :xxx: ... :END:
      (org-fold-hide-drawer-all)
    (org-hide-drawer-all))
  (my/org-hide-comment-blocks))

(add-hook 'org-mode-hook #'my/org-startup-visibility)
;; If the above hook seems to have no effect, defer it slightly:
;;
;; (add-hook 'org-mode-hook
;;           (lambda () (run-with-idle-timer 0 nil
;; 					  #'my/org-startup-visibility (current-buffer))))
