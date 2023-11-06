;;;
;;; browse-url configuration
;;;

(eval-when-compile (require 'browse-url))

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
  "Open URL or call find-file with the text at the point"
  (interactive "P")
  (let ((w (thing-at-point 'url)) url file)
    (if w
        (progn
          (setq url (url-generic-parse-url w))
          (let ((ut (url-type url)))
            (if (or (null ut) (string-equal ut "file"))
                (setq file (url-filename url)
                      url nil))))
      (setq w (thing-at-point 'filename))
      (if w
          (setq file w)))

    (let ((current-prefix-arg arg))
      (if file
          (call-interactively (lookup-key (current-global-map)
                                          [(control ?x) (control ?f)]))
        (if url
            (call-interactively #'browse-url))))))

(global-set-key [(control ?c) (control ?o)] #'browse-url-dwim)
