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
