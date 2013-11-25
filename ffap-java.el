(defvar ffap-java/source-directories
  (car (remove nil (list
                    (ffap-java/java-home-from-env)
                    (ffap-java/java-home-redhat)
                    (ffap-java/java-home-darwin)
                    (ffap-java/java-home-gentoo))))
  "List of elements separated by ':' for Java sources

Each elements must be a directory name or a jar archive name")

(defvar ffap-java/tmp-directory "/tmp/java-src/"
"Extracted sources from jar archive will be stored here.")

(defvar ffap-java/debug-buffer (get-buffer-create "*ffap-java*"))

(defun ffap-java/shell-command-to-string (command &optional raw)
  "Safe version of `shell-command-to-string'

It returns nil when the command failed (nonzero exit status).
If RAW is non-nil, the trailing newline and whitespaces are removed."
  (with-temp-buffer
    (let ((exit (process-file shell-file-name nil
                              (list (current-buffer) nil) nil
                              shell-command-switch command)))
      (when (eq exit 0)
        (if raw
            (buffer-substring-no-properties (point-min) (point-max))
          (replace-regexp-in-string
           "[[:space:]\n\r]*$" ""
           (buffer-substring-no-properties (point-min) (point-max))))))))


(defun ffap-java/java-home-from-env ()
  "Return a pathname of Java source archive from $JAVA_HOME"
  (let ((home (getenv "JAVA_HOME")))
    (when home
      (let ((zip (concat (file-name-as-directory home) "src.zip"))
            (jar (concat (file-name-as-directory home) "src.jar")))
        (cond ((file-readable-p zip) zip)
              ((file-readable-p jar) jar)
              nil)))))

(defun ffap-java/java-home-gentoo ()
  "Return a pathname of Java source archive in Gentoo Linux"
  (let ((home (ffap-java/shell-command-to-string "java-config -o")))
    (when home
      (let ((zip (concat (file-name-as-directory home) "src.zip"))
            (jar (concat (file-name-as-directory home) "src.jar")))
        (cond ((file-readable-p zip) zip)
              ((file-readable-p jar) jar)
              nil)))))

(defun ffap-java/java-home-redhat ()
  "Return a pathname of Java source archive in Redhat-based Linux"
  ;; It seems that /usr/java/default is the symlink to the actual java
  ;; home in CentOS (confirmed in version 6.3 from
  ;; /etc/centos-release)
  ;;
  ;; TODO: Maybe parsing "alternative --display java" is better??
  (let ((home "/usr/java/default"))
    (let ((zip (concat (file-name-as-directory home) "src.zip"))
          (jar (concat (file-name-as-directory home) "src.jar")))
      (cond ((file-readable-p zip) zip)
            ((file-readable-p jar) jar)
            nil))))

(defun ffap-java/java-home-darwin ()
  "Return a pathname of Java source archive in OSX, darwin"
  (let ((home (ffap-java/shell-command-to-string "/usr/libexec/java_home")))
    (when home
      (let ((zip (concat (file-name-as-directory home) "src.zip"))
            (jar (concat (file-name-as-directory home) "src.jar")))
        (cond ((file-readable-p zip) zip)
              ((file-readable-p jar) jar)
              nil)))))


(defun ffap-java/join (root &rest dirs)
  (if (not dirs)
      root
    (apply 'ffap-java/join
           (concat (file-name-as-directory root) (car dirs))
           (cdr dirs))))

(defun ffap-java/zip-has-file? (archive file)
  "Return t if ARCHIVE contains the FILE"
  (eq (call-process-region 1 1  "/bin/sh" nil nil nil "-c"
                           ;; unzip can handle .jar file
                           (format "unzip -t \"%s\" \"%s\"" archive srcpath))
      0))


(defun ffap-java/buffer-from-archive (archive source)
  "Create a new read-only buffer that has SOURCE from ARCHIVE."
  (let* ((buffer-name (file-name-nondirectory source))
         (buf (get-buffer-create (format "*%s*" buffer-name))))
    (with-current-buffer buf
      (when (not buffer-read-only)
        (erase-buffer)
        (let ((cmd (format "unzip -p \"%s\" \"%s\""
                                               archive source)))
          (when (eq (call-process-region (point-min) (point-max) "/bin/sh"
                                         'delete t 'display "-c" cmd)
                  0)
            (goto-char (point-min))
            (set-buffer-modified-p nil)
            (java-mode)
            (view-mode)
            buf))))))


(defun ffap-java/log (message &rest args)
  (when ffap-java/debug-buffer
    (with-current-buffer ffap-java/debug-buffer
      (goto-char (point-max))
      (insert (apply 'format message args))
      (newline))))



(defun ffap-java/source-from-archive (archive source)
  "Create a new file for the SOURCE from ARCHIVE and return the pathname"
  ;; TODO: use some buffer to leave a log message from unzip
  (let ((target (concat (file-name-as-directory ffap-java/tmp-directory)
                        source)))
    (if (and (not (file-directory-p target))
             ;; If TARGET is a directory name, we are not sure that
             ;; the directory contains all files from ARCHIVE.  Thus,
             ;; return the existing TARGET iff TARGET is a regular
             ;; file.
             (file-exists-p target))
        target
      (let ((cmd (format "unzip -n -d \"%s\" \"%s\" \"%s\""
                         ffap-java/tmp-directory
                         (expand-file-name archive)
                         (if (string-equal (substring source -1) "/")
                             (concat source "*")
                           source))))
        (ffap-java/log "exec: %s" cmd)
        (if (eq (call-process-region (point-min) (point-min) "/bin/sh"
                                     nil ffap-java/debug-buffer nil "-c"
                                     cmd)
                0)
            target)))))


(defun ffap-java/file-name-from-package (name)
  "Guess the filename from the package name, NAME"
  ;; TODO: Java's static import name is not supported.
  (let* ((wildcard nil)
         (src (apply 'ffap-java/join
                     (mapcar (lambda (tok)
                               (if (string-equal tok "*")
                                   (and (setq wildcard t) "")
                                 tok))
                             (split-string name "\\.")))))
    (if wildcard
        src
      (concat src ".java"))))


(defun ffap-java-mode (name)
  "Return a pathname for the Java package, NAME"
  ;; TODO: make a list of matching functions, and try each function from them
  (catch 'found
    ;; TODO: search the current directory for the name
    (let ((src (ffap-java/file-name-from-package name)))
      (message "src: %s" src)
      (dolist (home ffap-java/source-directories)
        (if (file-directory-p home)
            (let ((srcpath (concat (file-name-as-directory home) src)))
              (if (file-directory-p srcpath)
                  (throw 'found srcpath)
                (if (file-exists-p srcpath)
                    (throw 'found srcpath))))
          (let ((target (ffap-java/source-from-archive home src)))
            (when target
              (throw 'found target))))))))


(eval-after-load "ffap"
  '(add-to-list 'ffap-alist '(java-mode . ffap-java-mode)))


(provide 'ffap-java)
