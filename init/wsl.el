;; -*-emacs-lisp-*-

;;;
;;; WSL customization
;;;

(defvar wsl-open-program "/mnt/c/Windows/explorer.exe"
  "Program to open a file, or URL")

(defvar wsl-path-program-name "/usr/bin/wslpath"
  "an absolute pathname of wslpath(1)")
(defvar wsl-cmd-program-name "/mnt/c/WINDOWS/system32/cmd.exe"
  "an absolute pathname of cmd.exe(1)")

(defun wsl-path (uri)
  "Translate WSL path to Windows path"
  (if (eq (elt uri 0) ?~)
      (setq uri (expand-file-name uri)))
  (let ((u (url-generic-parse-url uri)))
    (if (or (null (url-type u))
            (string-equal (url-type u) "file"))
        (with-temp-buffer
          (call-process wsl-path-program-name
                        nil
                        (list (current-buffer) nil)
                        nil
                        "-w"
                        (url-filename u))
          (string-trim-right (buffer-substring-no-properties
                              (point-min) (point-max))))
      uri)))

(defun wsl-open (uri &optional _new-window)
  "Open a file using Windows cmd.exe, similar to xdg-open(1).

The optional argument NEW-WINDOW is not used.  This function
can be used for `browse-url-browser-function'."
  (let ((path (wsl-path uri)))
    (start-process (concat "WSL:cmd.exe " uri)
                   nil
                   wsl-cmd-program-name
                   "/c"
                   "start"
                   path)))

(defun wsl-host-address ()
  ;; https://learn.microsoft.com/en-us/windows/wsl/networking
  (with-temp-buffer
    (insert-file-contents "/etc/resolv.conf")
    (when (re-search-forward "^nameserver\\s-\\([a-z0-9A-Z.:/]+\\)$" nil t)
      (match-string 1))))


(eval-when-compile
  (require 'browse-url))

(defun wsl-browse-url (url &optional _new-window)
  "On WSL, ask Windows cmd.exe to load URL

The optional argument NEW-WINDOW is not used.  This function
can be used for `browse-url-browser-function'."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (start-process (concat "WSL:cmd.exe " url)
                   nil
                   wsl-cmd-program-name
                   "/c"
                   "start"
                   url)))
(setq browse-url-browser-function 'wsl-browse-url)

(when (display-graphic-p)
  ;; These configuration seems to work in
  ;; GNU Emacs 24.1.1 (x86_64-apple-darwin, NS apple-appkit-1038.36)
  ;; of 2012-06-11 on bob.porkrind.org

  ;; default font family
  ;; (set-face-attribute 'default nil :family "Monaco")
  ;; (set-face-attribute 'default nil :family "Inconsolata")

  ;; default font size
  ;;
  ;; WARNING: depending on the font family, some height value may
  ;; cause a broken frame display; that is, the beginning of the
  ;; buffer is not visible.
  ;; (set-face-attribute 'default nil :height 135)
  ;; (set-face-attribute 'default nil :height 160)

  ;;(set-fontset-font t 'unicode (font-spec :size 20.0))
  ;; You may add :size POINT in below font-spec if you want to use
  ;; specific size of Hangul font regardless of default font size

  (when (locate-library "fontutil")
    (require 'fontutil))

  (when (eq 0 (string-to-number (or (getenv "EMACS_NOFONT") "0")))
    (fontutil/set-font "dejavu-18")))

(with-eval-after-load "tex"
  (TeX-global-PDF-mode)
  (add-to-list 'TeX-command-list
               '("View" "/mnt/c/Windows/explorer.exe %o"
                 TeX-run-discard-or-function t t :help "Run Viewer")))

(setq ;; browse-url-generic-program wsl-open-program
      browse-url-browser-function #'wsl-open)


(with-eval-after-load "dired-x"
  (setq dired-guess-shell-alist-user
        (append dired-guess-shell-alist-user
                '(("\\.xbm\\'" wsl-open-program) ; You may need to install a viewer application like ToyViewer.
                  ("\\.png\\'" wsl-open-program)
                  ("\\.jpe?g\\'" wsl-open-program)
                  ("\\.tiff?\\'" wsl-open-program)
                  ("\\.pdf\\'" wsl-open-program)
                  ("\\.gnuplot\\'" "gnuplot")
                  ("\\.svg\\'" wsl-open-program)
                  ("\\.e?ps\\'" wsl-open-program)
                  ("\\.docx?\\'" wsl-open-program)
                  ("\\.pptx?\\'" wsl-open-program)
                  ("\\.xlsx?\\'" wsl-open-program)
                  ("\\.dmg\\'" wsl-open-program)
                  ("\\.pkg\\'" wsl-open-program)
                  ("\\.otf\\'" wsl-open-program)
                  ("\\.ttf\\'" wsl-open-program)
                  ;;("\\.msg\\'" "msgconvert") ; https://github.com/mvz/email-outlook-message-perl
                  ;;("\\.mobi\\'" "open -a ebook-viewer *") ; Install Calibre for ebook-viewer
                  ;;("\\.e?ps\\.g?z\\'" "gunzip -qc * | open -a Preview -f")
                  ;;("\\.e?ps\\.Z\\'" "zcat * | open -a Preview -f")
                  ))))
