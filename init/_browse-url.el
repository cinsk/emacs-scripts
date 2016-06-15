;;;
;;; browse-url configuration
;;;

(require 'browse-url)

(defun browse-url-w3m (url &optional new-window)
  (interactive (browse-url-interactive-arg "W3M URL: "))
  (w3m url))

(cond ((memq system-type '(windows-nt ms-dos cygwin))
       ;; Use system default configuration
       nil)

      ((or (display-graphic-p)
           ;; TODO: If DISPLAY environment variable is wrong,
           ;;       xset will hang!
           (= (call-process-shell-command "xset q") 0))
       ;; Even if (display-graphic-p) returns nil,
       ;; it may be possible to launch X application.
       (cond ((executable-find "chromium")
              (setq browse-url-browser-function 'browse-url-generic
                    browse-url-generic-program (executable-find "chromium")))
             ((executable-find browse-url-firefox-program)
              (setq browse-url-browser-function 'browse-url-firefox))))

      ((fboundp 'w3m)
       (setq browse-url-browser-function 'browse-url-w3m)))

(add-to-list 'browse-url-filename-alist
             '("\\`/home/\\([^/]+\\)/public_html/\\(.*\\)\\'" .
               "http://localhost/~\\1/\\2"))

;; gentoo: /var/www/localhost/htdocs
;; ubuntu: /var/www/
;; centos: /var/www/html/  /var/www/cgi-bin/
(add-to-list 'browse-url-filename-alist
             '("\\'/var/www/localhost/htdocs/\\(.*\\)\\'" .
               "http://localhost/\\1"))

(defun browse-url-dwim (&optional arg)
  "Open URL.

If the protocol is either http or https, call `browse-url'.
If the URL ends with one of HTML file extension, call `browse-url'.
Otherwise, call the command that is bound to 'C-x C-f'."
  (interactive "P")
  (let ((url (or (thing-at-point 'url) (thing-at-point 'filename))))
    (if (and url (or (string-match "\\`https?:/" url)
                     (string-match "\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" url)))
        (call-interactively #'browse-url)
      (call-interactively (lookup-key (current-global-map) [(control ?x) (control ?f)])))))

(global-set-key [(control ?c) (control ?o)] #'browse-url-dwim)
