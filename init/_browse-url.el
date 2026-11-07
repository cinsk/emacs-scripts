;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;
;;; browse-url configuration
;;;


(require 'browse-url)
(require 'thingatpt)
(require 'url-parse)

;; Map local file names to the URLs served by the local web server, so that
;; `browse-url-file-url' (and thus `browse-url-of-file') produces http URLs.
;; `add-to-list' pushes to the front, so these win over the default entries.
(add-to-list 'browse-url-filename-alist
             '("\\`/home/\\([^/]+\\)/public_html/\\(.*\\)\\'" .
               "http://localhost/~\\1/\\2"))

;; gentoo: /var/www/localhost/htdocs
;; ubuntu: /var/www/
;; centos: /var/www/html/  /var/www/cgi-bin/
(add-to-list 'browse-url-filename-alist
             '("\\`/var/www/localhost/htdocs/\\(.*\\)\\'" .
               "http://localhost/\\1"))

(defun browse-url-dwim (&optional arg)
  "Open the URL or file at point in a web browser.
In a Dired buffer, browse the file on the current line (works for files
in subdirectories inserted with `i').
If the text at point is a URL with a scheme other than \"file\", browse it.
Otherwise treat the text as a file name (or a file: URL), and browse that
file, honoring `browse-url-filename-alist'.
With prefix ARG, toggle `browse-url-new-window-flag' for this call."
  (interactive "P")
  (let* ((dired-file (and (derived-mode-p 'dired-mode)
                          ;; Full path, correct even inside an inserted
                          ;; subdirectory (dired `i'); nil if no file on line.
                          (dired-get-filename nil t)))
         (text   (and (not dired-file) (thing-at-point 'url t)))
         (parsed (and text (url-generic-parse-url text)))
         (type   (and parsed (url-type parsed)))
         (new-window (if arg
                         (not browse-url-new-window-flag)
                       browse-url-new-window-flag))
         url)
    (cond
     ;; Dired: use the file on the current line, with its real directory.
     (dired-file
      (setq url (browse-url-file-url (expand-file-name dired-file))))
     ;; Real URL: http, https, ftp, mailto, ...
     ((and type (not (string-equal type "file")))
      (setq url text))
     ;; file: URL or plain file name.
     (t
      (let ((file (if type
                      (url-unhex-string (url-filename parsed))
                    (thing-at-point 'filename t))))
        (unless file
          (user-error "No URL or file name at point"))
        (setq file (expand-file-name file))
        (unless (file-exists-p file)
          (user-error "File does not exist: %s" file))
        (setq url (browse-url-file-url file)))))
    (browse-url url new-window)))

;; Requires Emacs 29+ for `keymap-global-set'.  Note that C-c C-o is bound
;; locally in several modes (org, markdown, ...), which overrides this.
(keymap-global-set "C-c C-o" #'browse-url-dwim)


