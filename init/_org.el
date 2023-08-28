
;; -*-emacs-lisp-*-

;;;
;;; Org mode
;;;

(eval-when-compile
  (require 'org)
  (require 'org-table))

(unless (fboundp 's-trim)
  ;; I stole these three string functions from s package.
  ;; See https://github.com/magnars/s.el for more
  (defun s-trim-left (s)
    "Remove whitespace at the beginning of S."
    (save-match-data
      (if (string-match "\\`[ \t\n\r]+" s)
          (replace-match "" t t s)
        s)))

  (defun s-trim-right (s)
    "Remove whitespace at the end of S."
    (save-match-data
      (if (string-match "[ \t\n\r]+\\'" s)
          (replace-match "" t t s)
        s)))

  (defun s-trim (s)
    "Remove whitespace at the beginning and end of S."
    (s-trim-left (s-trim-right s))))

;;
;; Disable the prompt for evaluation of code block
;;
(setq org-confirm-babel-evaluate nil)


;;
;; generate link description automatically
;;
(setq org-link-abbrev-alist
      '(("google" . "http://www.google.com/search?q=%s")
        ("rfc" . "http://www.rfc-editor.org/rfc/rfc%s.txt")
        ("man" . "%(orglink/url-man)")
        ("java" . "%(orglink/url-java)")))

(defvar org-link-javadoc-root-alist
      '(("org.springframework.boot." . "http://docs.spring.io/spring-boot/docs/current/api/")
        ("org.springframework." . "http://docs.spring.io/spring/docs/current/javadoc-api/")
        ("org.apache.log4j." . "http://www.slf4j.org/apidocs/")
        ("org.slf4j." . "http://www.slf4j.org/apidocs/")
        ("org.junit." . "http://junit.sourceforge.net/javadoc/")
        ("lombok." . "https://projectlombok.org/api/")
        ("java." . "http://docs.oracle.com/javase/8/docs/api/"))
"List of java package root and their base URLs

Unlike other alist type, the order of entries are important."
      )


(defun orglink/starts-with (s1 s2)
  "Return non-nil if S1 starts with S2."
  (let ((s1len (length s1))
        (s2len (length s2)))
    (when (>= s1len s2len)
      (string-equal s2 (substring s1 0 s2len)))))

(defun orglink/url-java (tag)
  ;; "org.springframework.web.SpringServletContainerInitializer"
  (save-match-data
    (let ((name (replace-regexp-in-string "\\." "/" tag)))
      (catch 'orglink/found
        (dolist (ent org-link-javadoc-root-alist)
          (let ((pkg (car ent))
                (base (cdr ent)))
            (if (orglink/starts-with tag pkg)
                (throw 'orglink/found (concat base name ".html")))))))))


(defun orglink/url-man (spec)
  "Return an URL for man pages.

SPEC is in the form of \"NAME(SECTION)\" or \"SECTION NAME\",
where NAME is the keyword and SECTION is the manual section
number.

This function is used for the `org-mode' file to provide an link abbreviation.
For example, add following line in your org file:

#+LINK: man %(org-man-link)"
  (save-match-data
    (let (name sect)
      (if (string-match
           "\\`[[:blank:]]*\\([^[:blank:]]+\\)[[:blank:]]*(\\([^)]+\\))"
           spec)
          (setq name (match-string 1 spec)
                sect (match-string 2 spec))
        (if (string-match
             "\\`[[:blank:]]*\\([^)]+\\)[[:blank:]]*\\([^[:blank]]+\\)" spec)
            (setq name (match-string 2 spec)
                  sect (match-string 1 spec))
          (error "Invalid manual format, %s" spec)))
      ;; We can't guarantee that SECT is consists of numbers only.
      ;; e.g.  there can be "scanf (3p)".
      (setq sect (s-trim sect))
      (format "http://man7.org/linux/man-pages/man%s/%s.%s.html"
              sect name sect))))



(defvar orglink/link-desc-alist
      '(("google" . nil)
        ("rfc" . orglink/desc-rfc)
        ("java" . orglink/desc-java))
      "Alist of org linkwords and their description generating functions

Each description function takes three parameters; If the user
gives \"LINKWORD:PATH\", then the first parameter receives
\"PATH\", the second parameter receives \"LINKWORD\", and the
third parameter receives the DESC value.  Note that rest of
parameters except the first one are optional.

See `org-make-link-description-function' for DESC value.
")

(defun orglink/description-function (link desc)
  (save-match-data
    (if desc desc
      (let* ((scheme (if (string-match "\\`\\([^:]+\\):\\(.*\\)\\'" link)
                        (match-string 1 link)))
             (path (match-string 2 link))
             (entry (assoc scheme orglink/link-desc-alist))
             (proc  (cdr entry)))
        (if (or (not proc) (not (fboundp proc))) path
          (or (funcall proc path scheme desc)
              path))))))

(setq org-make-link-description-function 'orglink/description-function)

(defun orglink/desc-rfc (path &optional scheme desc)
  ;; path will be something like "0434" or "23"
  (let ((index (string-to-number path)))
    (unless (> index 0)
      (error "Invalid RFC number, %s" index))
    (format "RFC %s" path)))

(defun orglink/desc-java (path &optional scheme desc)
  ;; path will be something like spring:web.springframwork.aop.support.AopUtils
  (when (string-match
         "\\`\\(?:.*?\\.\\([^.]+\\)\\|\\([^.]+\\)\\)\\'" path)
    (or (match-string 1 path) (match-string 2 path))))



(defun different-command-p (command name)
  "Return t iff COMMAND is a command and has different from NAME"
  (and (commandp command)
       (not (eq name command))
       ;string-match name (symbol-name command)))
       command))

(defun call-next-command (keys not-this-command)
  "Interactively call the command that has a binding of KEYS, but
not NOT-THIS-COMMAND"
  (let ((command (catch 'found
                   (mapc (lambda (map)
                           (let ((cmd (different-command-p
                                       (lookup-key map keys)
                                       not-this-command)))
                             (when cmd
                               (throw 'found cmd))))
                          (current-minor-mode-maps))
                   (or (different-command-p (lookup-key (current-local-map)
                                                        keys)
                                            not-this-command)
                       (different-command-p (lookup-key (current-global-map)
                                                        keys)
                                            not-this-command)))))
  (when (commandp command)
    (call-interactively command))))

(defun org-insert-word-joiner-or-space ()
  (interactive)
  (if (bolp)
      ;; If this trick used with yas-snippet, and if TAB is pressed
      ;; where the point is in the begining of the line, then the following
      ;; error occurs: lisp nesting exceeds `max-lisp-eval-depth'.
      (call-interactively
       (lookup-key org-mode-map (this-command-keys-vector)))
      (save-match-data
        (when (looking-back "\\([=/]\\)\\(.*\\)\\1\\([^[:space:]\u2060]+\\)"
                            (line-beginning-position) 'greedy)
          ;; 2nd match => emphasised phrase (e.g. =code=)
          ;; 3rd match => partial word appended
          (save-excursion
            (goto-char (match-beginning 3))
            (insert-char #x2060)))
        (call-next-command (this-command-keys-vector)
                           'org-insert-word-joiner-or-space))))

(when nil
  (defvar org-wordjoin-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [?\ ] 'org-insert-word-joiner-or-space)
      (define-key map [(control ?j)] 'org-insert-word-joiner-or-space)
      (define-key map [(control ?m)] 'org-insert-word-joiner-or-space)
      (define-key map [(tab)] 'org-insert-word-joiner-or-space)
      map)
    "Keymap for `org-wordjoin-mode'"))

(defvar org-wordjoin-genuine-map)
(defvar org-wordjoin/override-keys
  '([?\ ] [(control ?j)] [(control ?m)] [(tab)])
  "List of key sequences for `org-wordjoin-mode'.")


(define-minor-mode org-wordjoin-mode
  "Enable automatic insertion of word joiner"
  nil
  " WordJoin"
  ;; :keymap org-wordjoin-mode-map
  :keymap nil
  (make-local-variable 'org-wordjoin-genuine-map)
  (if org-wordjoin-mode
      (progn
        (setq org-wordjoin-genuine-map (org-wordjoin/genuine-command-keymap))
        (org-wordjoin/install-wordjoin-commands))
    (org-wordjoin/restore-command-keymap org-wordjoin-genuine-map)))

(defun org-wordjoin/install-wordjoin-commands ()
  "Install keybindings of `org-wordjoin-mode'."
  (dolist (key org-wordjoin/override-keys)
    (let ((cmd (lookup-key org-mode-map key)))
      (if cmd
          (define-key org-mode-map key `(lambda ()
                                          (interactive)
                                          (org-wordjoin/insert-command
                                           (quote ,cmd))))
        (define-key org-mode-map key (lambda ()
                                       (interactive)
                                       (org-wordjoin/insert-command
                                        'self-insert-command)))))))

(defun org-wordjoin/restore-command-keymap (map)
  "Restore the genuine bindings from `org-wordjoin-mode'."
  (map-keymap (lambda (ev cmd)
                (define-key org-mode-map (vector ev) cmd)) map))

(defun org-wordjoin/genuine-command-keymap ()
  "Return a keymap with genuine bindings before `org-wordjoin-mode'."
  (let ((map (make-sparse-keymap)))
    (dolist (key org-wordjoin/override-keys)
      (let ((cmd (lookup-key org-mode-map key)))
        (define-key map key cmd)))
    map))

(defun org-wordjoin/insert-command (&optional command)
  "Insert word joiner, and call COMMAND interactively if any."
  (save-match-data
    (when (looking-back "\\([=/]\\)\\(.*\\)\\1\\([^[:space:]\u2060]+\\)"
                        (line-beginning-position) 'greedy)
      ;; 2nd match => emphasised phrase (e.g. =code=)
      ;; 3rd match => partial word appended
      (save-excursion
        (goto-char (match-beginning 3))
        (insert-char #x2060))))
  (and command
       (call-interactively command)))

(with-eval-after-load "org"
  ;;
  ;; Enable evaluation code block of certain languages
  ;;
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((emacs-lisp . t)
     (R . t)
     (python . t)
     (ruby . t)
     (dot . t)
     ,(when (locate-library "ob-go") '(go . t))
     ; (sh . t) -- ob-sh renamed to ob-shell
     (shell . t)))

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
  (cinsk/move-key org-mode-map [(control tab)] [(control x) (control tab)])

  ;; Move the binding of `org-deadline' from "C-c C-d" to "C-c
  ;; C-S-d", since I'vd used the keybinding for
  ;; `delete-chars-forward-with-syntax'.
  (cinsk/move-key org-mode-map [(control ?c) (control ?d)]
                  [(control ?c) (control shift ?d)])

  (define-key org-mode-map [(control c) (control ?\\)]
    'org-table-convert-from-lines)

  (define-key org-mode-map [(control c) ?t] 'org-todo)

  (define-key org-mode-map [(control c) ?s] 'cinsk/org-sourcefy)

  ;; Shift + TAB in rdesktop, xmanager, emacs
  (define-key org-mode-map [(shift kp-tab)] 'org-shifttab)

  ;; unicode: word joiner
  ;;
  ;; To mark-up certain text (e.g. /"hello"/ga), place 'word joiner'
  ;; character in /"hello"/<!>ga, where <!> represents the position for the
  ;; word joiner.
  (define-key org-mode-map [(control c) (control ?\;)]
    '(lambda ()
       (interactive)
       (ucs-insert #x2060)))
  ;; unicode: zero width space
  ;;
  ;; To mark-up certain text (e.g. ="hello"=), place 'zero width space'
  ;; character in =<!>"hello"<!>=, where <!> represent the two position
  ;; for the zero width space character.
  (define-key org-mode-map [(control c) (control ?\')]
    '(lambda ()
       (interactive)
       (ucs-insert #x200B)))

  ;;(add-to-list 'org-mode-hook (lambda ()
  ;; (org-wordjoin-mode 1)))

  (when (fboundp 'graphviz-dot-mode)
    (let ((dotpair (assoc "dot" org-src-lang-modes)))
      (and dotpair
           (setcdr dotpair 'graphviz-dot))))

  (cond ((eq system-type 'gnu/linux)
         (add-to-list 'org-file-apps '("pdf" . "acroread %s"))
         (add-to-list 'org-file-apps '("ps" . "ggv %s")))
        ((eq system-type 'darwin)
         ;; pdf already handled by Preview
         (add-to-list 'org-file-apps '("ps" . "open %s"))))
  (when (fboundp 'browse-url)
    ;; "export as HTML and open" (a.k.a `C-c C-e h o') uses
    ;; `org-open-file' to open HTML, which uses $HOME/.mailcap,
    ;; which usually uses lynx(1) to open HTML file, which is not
    ;; what I want.  It's better to use `browse-url' to determine
    ;; the suitable browser.
    (add-to-list 'org-file-apps
                 '("\\.x?html?\\'" . (lambda (file link) (browse-url file)))))

  ;; Org mode requires font-locking on every org buffer
  ;; Since I use global-font-lock-mode, below sexp is not necessary.
  ;;
  ;; (add-hook 'org-mode-hook 'turn-on-font-lock)

  ;; According to http://orgmode.org/org.html#Conflicts,
  ;; filladapt.el is not working well with Org, so disable it in
  ;; org-mode.
  (add-hook 'org-mode-hook '(lambda ()
                              (if (fboundp 'turn-off-filladapt-mode)
                                  (turn-off-filladapt-mode))))

  ;; org-hide-leading-stars should be set before loading org-mode.
  (setq org-hide-leading-stars t)
  (setq org-odd-levels-only t)
  ;; (setq org-agenda-include-diary t)

  ;; If org file loaded with folding, comparing files with ediff
  ;; is very unhandy, thus starting with everything is shown
  (setq org-hide-block-startup nil)
  (setq org-startup-folded 'showeverything)

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
                     '(" \t('\"{\\\u2060"
                       "- \t.,:!?;'\")}\\\u2060"
                       " \t\r\n,\"'⁠"
                       "." 1)))

  (and (boundp 'org-drawers)
       (add-to-list 'org-drawers "COMMENT"))
  )


(when (locate-library "org-version")
  ;; From Org-Mode version 7.9.2, there is no need to load
  ;; 'org-install.  Worse, it will display a warning, which makes me
  ;; uncomfortable.  Following sexp will load 'org-install if the
  ;; installed version is lower than 7.9.2.
  (require 'org-version)
  (if (and (fboundp 'org-release)
           (string-lessp (org-release) "7.9.2"))
      ;; Around orgmode version 7.9.2 and version 7.9.3, if the module,
      ;; org-version is loaded, then some of org-related symbols are
      ;; accessible (e.g. `org-mode-map').
      ;;
      ;; From orgmode 7.9.4, however, `org-mode-map' is not loaded unless
      ;; org module is loaded.  Thus, I loaded org module below.
      ;;
      ;; TODO: Move all of org specific configuration below to the
      ;;       `eval-after-load' if possible
      ;; (require 'org)
    ;; nil                               ; do nothing
    (require 'org-install)))


(when (string-lessp emacs-version "22.2")
  ;; From GNU Emacs version 22.2, ".org" extension use Org mode by default.
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)))

(global-set-key [(control c) ?a] 'org-agenda)
(global-set-key [(control c) ?l] 'org-store-link)
(global-set-key [(control c) ?b] 'org-iswitchb)
(global-set-key [(control c) ?\"] 'org-capture)

;;(org-remember-insinuate)
(global-set-key [f8] 'org-capture)

(defun cinsk/org-refresh-agenda-files (&optional directory)
  "Refresh `org-agenda-files' to the ORG files in DIRECTORY.

Filenames that matches Dropbox conflict will not be included."
  (let ((dir (or directory org-directory)))
    (when (file-accessible-directory-p dir)
      (setq org-agenda-files
            (let ((files (directory-files dir t
                                          "\\`[^.#].*\\.org\\'"))
                  (conflict-regexp
                   "(.*?[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}).org$"))
              (delete nil (mapcar
                           (lambda (filename)
                             (if (string-match conflict-regexp
                                               filename)
                                 nil
                               filename)) files)))))))

(let* ((org-path (getenv "ORG_PATH"))
       (my-org-directory (if org-path
                             org-path
                           (expand-file-name "~/agenda"))))
  ;; All of my org agena files are located in `my-org-directory'.
  (and (not (file-accessible-directory-p my-org-directory))
       (make-directory my-org-directory t))

  (when (string-prefix-p (file-truename "~/Dropbox")
                         (file-truename my-org-directory))
    ;; If `my-org-directory' is linked to a subdirectory of Dropbox
    ;; directory, then make the auto-save directory to be placed in
    ;; /tmp/.
    (add-to-list 'auto-save-file-name-transforms
                 (list
                  (concat "\\`"
                          (file-name-as-directory my-org-directory)
                          "\\([^/]*/\\)*\\([^/]*\\)\\'")
                  (concat (file-name-as-directory temporary-file-directory)
                          "\\2")
                  t)))

  (if (file-accessible-directory-p my-org-directory)
      (let ((notefile (path-join my-org-directory "notes.org")))
        ;; Install all .org files in `my-org-directory' if exists
        ;;
        ;; Currently `my-org-directory' is maintained by Dropbox.
        ;; I don't know what causes this, but Dropbox leaves some conflicted
        ;; copy in the form of "sample (XXX's conflicted copy 2009-10-15).org"
        ;; or "sample (XXX와(과) 충돌하는 사본 2013-02-22).org" in Korean.
        ;;
        ;; Thus, I'll add non-conflicted filenames into `org-agenda-files'.
        (cinsk/org-refresh-agenda-files my-org-directory)

        (setq org-default-notes-file notefile)
        (setq org-directory my-org-directory))
    (lwarn '(dot-emacs) :warning
           (format "cannot access org files in %s." my-org-directory))))

(setq org-capture-templates
      '(("w" "Work-related TODO" entry
         (file+headline (path-join org-directory "work.org")
                        "Tasks")
         "* TODO %? %T\n  %i\n  %a")
        ("p" "Personal Project-related TODO" entry
         (file+headline (path-join org-directory "pproject.org")
                        "Tasks")
         "* TODO %? %T\n  %i\n  %a")
        ("P" "Personal TODO" entry
         (file+headline (path-join org-directory "personal.org")
                        "Tasks")
         "* TODO %? %T\n  %i\n  %a")
        ("e" "Emacs Tips" entry
         (file+headline (path-join user-emacs-directory "emacs-tips.org")
                        "Tips")
         "* TODO %? %T\n  %i\n  %a")
        ("j" "Personal Journal (date based)" entry
         (file+datetree (path-join org-directory "personal.org"))
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

(setq org-html-head "
<link rel=\"stylesheet\" type=\"text/css\"
      href=\"http://cinsk.github.io/bootstrap/css/bootstrap.css\"/>
<link rel=\"stylesheet\" type=\"text/css\"
      href=\"http://cinsk.github.io/bootstrap/css/bootstrap-responsive.css\"/>
<link rel=\"stylesheet\" type=\"text/css\"
      href=\"http://cinsk.github.io/bootstrap/css/docs.css\"/>
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
      (completing-read "mode: " cinsk/mode-name-list nil 'confirm initial
                       'cinsk/org-sourcefy-history))
    (region-beginning)
    (region-end)))

  (when (not (use-region-p))
    (setq begin (point) end (point)))

  (goto-char end)
  (unless (bolp)
    (newline))
  (insert "#+END_SRC\n")
  (push-mark)
  (goto-char begin)
  (unless (bolp)
    (newline))
  (insert (format "#+BEGIN_SRC %s\n" mode))
  (end-of-line 0)

  (when (use-region-p)
    ;; Run keyboard macro C-' C-' (`org-edit-special' and `org-edit-src-exit')
    ;; to indent the source block properly
    (kmacro-exec-ring-item (quote ("''" 0 "%d")) 1))
  (push mode cinsk/org-sourcefy-history))

;;
;; This setting causes org export to HTML failed.
;;
;; (setq org-todo-keywords
;;       '((sequence "TODO" "|" "DONE")
;;         (sequence "RESEARCH" "|" "DEVELOP" "|" "TEST" "|" "OP-DONE")
;;         (sequence "IMPL" "|" "REVIEW" | "RELEASE" "|" "SP-DONE")))
