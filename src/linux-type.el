;;; linuxtype.el --- Determine Linux Type and Version

;; Copyright (C) 2012  Seong-Kook Shin

;; Author: Seong-Kook Shin <cinsky@gmail.com>
;; Keywords: unix, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defconst linux-release-files
  '(("/etc/annvix-release" . annvix)
    ("/etc/arch-release" . arch-linux)
    ("/etc/arklinux-release" . arklinux)
    ("/etc/aurox-release" . aurox-linux)
    ("/etc/blackcat-release" . blackcat)
    ("/etc/cobalt-release" . cobalt)
    ("/etc/conectiva-release" . conectiva)
    ("/etc/debian_version" . debian)
    ("/etc/debian_release" . debian)
    ("/etc/fedora-release" . fedora)
    ("/etc/gentoo-release" . gentoo)
    ("/etc/immunix-release" . immunix)
    ("/etc/knoppix_version" . knoppix)
    ("/etc/lfs-release" . linux-from-scratch)
    ("/etc/linuxppc-release" . linuxppc)
    ("/etc/mandrake-release" . mandrake)
    ("/etc/mandriva-release" . mandrake)
    ("/etc/mandrakelinux-release" . mandrake)
    ("/etc/mklinux-release" . mklinux)
    ("/etc/nld-release" . novell)
    ("/etc/pld-release" . pld)
    ("/etc/redhat-release" . redhat)
    ("/etc/redhat_version" . redhat)
    ("/etc/slackware-version" . slackware)
    ("/etc/slackware-release" . slackware)
    ("/etc/e-smith-release" . sme-server)
    ;;("/etc/release" . solaris-sparc)
    ;;("/etc/sun-release" . sun-jds)
    ("/etc/SuSE-release" . suse)
    ("/etc/novell-release" . suse)
    ("/etc/sles-release" . suse-es9)
    ("/etc/tinysofa-release" . tiny-sofa)
    ("/etc/turbolinux-release" . turbolinux)
    ("/etc/lsb-release" . ubuntu)
    ("/etc/ultrapenguin-release" . ultrapenguin)
    ("/etc/UnitedLinux-release" . unitedlinux)
    ("/etc/va-release" . va-linux/rh-vale)
    ("/etc/yellowdog-release" . yellow-dog))
  "Alist of release file and the linux distribution types from
http://linuxmafia.com/faq/Admin/release-files.html")

(defvar linux-release-file nil
  "")

(defconst linux-type nil
  "The value is a symbol indicating the type of Linux system you are using.")

(defun linux-type (&optional init-global)
  "Get the symbol representing the Linux type"
  (catch 'found
    (dolist (ent linux-release-files)
      (if (file-exists-p (car ent))
          (progn
            (if init-global
                (setq linux-release-file (car ent)
                      linux-type (cdr ent)))
            (throw 'found (cdr ent)))))))

;;;
;;; /etc/gentoo-release:
;;;   Gentoo Base System release 2.0.2
;;;
;;; /etc/lsb-release:
;;;   DISTRIB_ID=Ubuntu
;;;   DISTRIB_RELEASE=10.04
;;;   DISTRIB_CODENAME=lucid
;;;   DISTRIB_DESCRIPTION="Ubuntu 10.04.3 LTS"
;;;
;;; /etc/redhat-release:
;;;   CentOS Release 5.7 (final)

  
(when (eq system-type 'gnu/linux)
  (linux-type 'init-global))

(provide 'linux-type)
;;; linuxtype.el ends here
