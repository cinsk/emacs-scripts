;; -*-emacs-lisp-*-

;;;
;;; Mail/News related configuration
;;;
(require 'smtpmail)
(require 'starttls)

(defvar default-imap-port 993
  "Default port number for the IMAP4 protocol")

(defvar default-imap-address "imap.gmail.com"
  "Default IMAP server address")

(defvar default-imap-stream 'ssl
  "Default IMAP connection method.  See possible value from
  `nnimap-stream'.")

(defvar default-pop3-ssl-port 995
  "Default port number for the POP3 protocol over TSL/SSL")

(defvar default-smtp-ssl-port 587
  "Default port number for the encrypted SMTP protocol.
Best used for `smtpmail-smtp-service' as the default value.")

(defvar default-smtp-port 25
  "Default port number for the SMTP protocol.
Best used for `smtpmail-smtp-service' as the default value.")

(defvar default-smtp-server "smtp.gmail.com"
  "Default SMTP server address")

(defvar company-firewall-on-effect nil
  "t if behind the infamous company firewall")

(when (string-match "^selune" system-name)
  (setq company-firewall-on-effect t))

;; Since `gnus-nntp-server' will override `gnus-select-method', force
;; `gnus-nntp-server' to nil.
(setq gnus-nntp-server nil)

;;(setq gnus-select-method '(nntp "news.kornet.net"))
;;(setq gnus-select-method '(nntp "public.teranews.com"))

;; The select method for `M-x gnus'.
(setq gnus-select-method '(nntp "news.easynews.com"))

(when company-firewall-on-effect
  ;; My company firewall does not allow out-going traffic except port 80/443.
  ;;
  ;; On machine inside of company, use alternative NNTP configuration.
  ;;
  ;; For external IMAP server, use ssh local port forwarding:
  ;;
  ;; localhost:8993 -> imap.gmail.com:993
  ;;
  (setq gnus-select-method '(nntp "proxy.news.easynews.com"
                                  (nntp-port-number 80))
        default-imap-port 8993
        default-imap-stream "network"
        default-imap-address "localhost"))

;; `C-u M-x gnus' will use the secondary select method.
;;(setq gnus-secondary-select-methods '(nntp "news.kornet.net"))
(setq gnus-secondary-select-methods
      `((nnfolder "")
        ;; (nnimap "cinsky"
        ;;         (nnimap-stream ,default-imap-stream)
        ;;         (nnimap-address "imap.gmail.com")
        ;;         (nnimap-server-port ,default-imap-port))
        ;; (nnimap "admin"
        ;;         (nnimap-stream ,default-imap-stream)
        ;;         (nnimap-address "imap.gmail.com")
        ;;         (nnimap-server-port ,default-imap-port))
        ))

;; If you need to use multiple SMTP accounts, read the
;; following articles:
;;
;;   http://linil.wordpress.com/2008/01/18/gnus-gmail/

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      mail-from-style nil
      user-full-name "Seong-Kook Shin"
      user-mail-address "cinsky@gmail.com"
      message-signature-file "~/.signature"
      smtpmail-debug-info t
      smtpmail-debug-verb t)

;;
;; TODO: 1. set T if the external `gnutls-cli' exists
;;       2. set nil if the external `starttls' exists
;;       3. show warning message that SMTP will be not working.
(setq starttls-use-gnutls t)

(if starttls-use-gnutls
    (let ((tls (locate-file "gnutls-cli" exec-path)))
      (if (and tls (file-executable-p tls))
          (setq starttls-gnutls-program tls)
        (progn
          (lwarn '(dot-emacs) :warning
                 "GNUTLS command is not found, SMTP may not work correctly")
          (setq starttls-use-gnutls nil)))))

(if (not starttls-use-gnutls)
    (let ((tls (locate-file "starttls" exec-path)))
      (if (and tls (file-executable-p tls))
          (setq starttls-program tls)
        (lwarn '(dot-emacs) :warning
               "STARTTLS command is not found, SMTP may not work correctly"))))


;; Extra argument to "gnutls-cli"
(setq starttls-extra-arguments nil)

(setq smtpmail-smtp-server default-smtp-server)
(setq smtpmail-smtp-service default-smtp-ssl-port)

;; SMTP Username and password is located in seperated file for the security.
;; The format of ~/.authinfo looks like:
;;
;;   machine imap.gmail.com login USER@gmail.com password PASSWORD port 993
;;   machine smtp.gmail.com login USER@gmail.com password PASSWORD port 587
;;
;; Make sure that ~/.authinfo has access mode 600.

(let ((netrc "~/.authinfo"))
  (if (file-readable-p netrc)
      (setq smtpmail-auth-credentials netrc)
    (lwarn '(dot-emacs) :warning
           "NETRC auth. file not exist, SMTP may not work correctly")
    (with-temp-file netrc)))

(setq smtpmail-starttls-credentials `((,smtpmail-smtp-server
                                       ,default-smtp-ssl-port
                                       nil nil)))


(defun complete-contact-address-internal ()
  (let ((name (completing-read "address: "
                               my-google-contacts
                               nil 'confirm)))
    (if (string-match "@" name)
        name
      (let ((found (assoc name my-google-contacts))
            (nam (if (string-match "\\(.*?\\) *([^)]*) *$" name)
                     (match-string 1 name)
                   name)))
        (format "%s <%s>" nam (cdr found))))))

(defun complete-contact-address (&optional arg)
  (interactive "P")
  (let ((address (complete-contact-address-internal))
        (pos (point)))
    (save-restriction
      (save-match-data
        (goto-char (point-min))
        (re-search-forward (regexp-quote mail-header-separator)
                           (point-max) t)
        (beginning-of-line)
        (let ((header-sep (point)))
          (if (>= pos header-sep)
              (progn
                (goto-char (point-min))
                (re-search-forward "^To:" header-sep t))
            (goto-char pos))
          (beginning-of-line)
          (if (or (re-search-forward "^[^[:blank:]][^:]*:[[:blank:]]*$"
                                     (line-end-position) t)
                  (re-search-forward "^[[:blank:]]+$" (line-end-position) t))
              (insert address)
            (beginning-of-line)
            (re-search-forward "[,[:blank:]]*$" (line-end-position) t)
            (replace-match (format ", %s" address))))))))

(eval-after-load "sendmail"
  '(progn
     (define-key mail-mode-map [(meta return)] 'complete-contact-address)

     (let ((contacts (concat (file-name-as-directory user-emacs-directory)
                             "contacts.el")))
       (when (file-exists-p contacts)
         (load-file contacts)))))
