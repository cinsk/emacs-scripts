;;; cinsk-wood.el --- wood color theme for Emacs

;; Copyright (C) 2005  Seong-Kook Shin <cinsk at gmail dot com>

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; As requested on the color-theme homepage on the Emacs Wiki:
;; http://www.emacswiki.org/cgi-bin/wiki/ColorTheme

;;; Code:

(require 'color-theme)

(add-to-list 'color-themes
	     '(color-theme-cinsk-wood "Cinsk Wood"
				      "Seong-Kook Shin <cinsk@gmail.com>"))

(defun color-theme-cinsk-wood ()
  "Seong-Kook Shin's wood theme."
  (interactive)
  (color-theme-install
   '(color-theme-cinsk-wood
     ((foreground-color . "burlywood")
      (background-color . "black"))
     ((CUA-mode-read-only-cursor-color . "dark grey")
      (help-highlight-face . info-xref)
      (list-matching-lines-buffer-name-face . bold))
     ;; general
     (default ((t (nil))))
     (button ((t (:bold t))))
     (fringe ((t (:background "SaddleBrown"))))
     (menu ((t (:background "brown" :foreground "grey"))))
     (modeline ((t (:background "IndianRed3" :foreground "white"
		    :box (:line-width 1 :style released-button)))))
     (mode-line-inactive ((t (:background "IndianRed4" :foreground "grey80"
                              :box (:line-width 1 :style released-button)))))
     
     (minibuffer-prompt ((t (:foreground "OrangeRed3"))))
     (tool-bar ((t (:background "pink"
                    :box (:line-width 1 :style released-button)))))
     (tooltip ((t (:background "lemon chiffon" :foreground "violet red"))))
     ;; isearch
     ;; (isearch ((t (:foreground "brown4" :background "palevioletred2"))))
     (isearch ((t (:foreground "beige" :background "palevioletred2"))))
     (isearch-lazy-highlight-face ((t (:foreground "red"))))
     ;; info-mode
     (header-line ((t (:background "sienna" :foreground "khaki"))))
     ;; calendar
     (calendar-today-face ((t (:foreground "lemon chiffon"))))
     (diary-face ((t (:bold t :foreground "yellow"))))
     (holiday-face ((t (:bold t :foreground "peru"))))
     ;; font-lock
     (font-lock-builtin-face ((t (:foreground "tan1"))))
     (font-lock-comment-delimiter-face ((t (:foreground "coral"))))
     (font-lock-comment-face ((t (:foreground "chocolate1"))))
     (font-lock-constant-face ((t (:foreground "DarkGoldenrod3"))))
     (font-lock-doc-face ((t (:foreground "coral"))))
     (font-lock-function-name-face ((t (:foreground "LemonChiffon1"))))
     (font-lock-keyword-face ((t (:foreground "IndianRed1"))))
     (font-lock-negation-char-face ((t (:foreground "red"))))
     (font-lock-preprocessor-face ((t (:foreground "sienna2"))))
     (font-lock-string-face ((t (:foreground "LightSalmon"))))
     (font-lock-type-face ((t (:foreground "LimeGreen"))))
     (font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
     (font-lock-warning-face ((t (:bold t :foreground "red"))))
     ;; cperl
     (cperl-array-face ((t (:bold t :foreground "tomato"))))
     (cperl-hash-face  ((t (:bold t :foreground "chocolate"))))
     (cperl-nonoverridable-face  ((t (:foreground "red"))))
     ;; makefiles
     (makefile-shell-face  ((t (:background "linen")))))))

(provide 'cinsk-wood)

;;; cinsk-wood.el ends here
