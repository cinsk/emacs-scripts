;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;
;;; Calendar, holidays and diary
;;;
;;; Loaded after _org.el (it uses `org-directory').
;;;
;;; Holidays come from two groups:
;;;
;;;   - `holiday-company-holidays': the company's official holidays,
;;;     marked with the standard `holiday' face.
;;;   - Korean (HQ) holidays: `korean-holidays' plus
;;;     `holiday-hq-holidays', named with `my/holiday-korean-prefix'
;;;     and marked with `my/calendar-korean-holiday'.  A date that is
;;;     also an official holiday keeps the official face.
;;;
;;; Diary entries live in an Org file, `org-agenda-diary-file'
;;; (~/agenda/diary.org), not in a plain `diary-file'.

(require 'seq)

(defvar org-directory)
(defvar org-overriding-default-time)
(defvar displayed-year)                 ; dynamically bound by the calendar
(declare-function diary-fancy-display "diary-lib")
(declare-function calendar-cursor-to-date "calendar")
(declare-function calendar-holiday-list "holidays")
(declare-function calendar-mark-visible-date "calendar")
(declare-function org-capture "org-capture")
(declare-function org-capture-get "org-capture")
(declare-function org-calendar-goto-agenda "org-agenda")
(declare-function my/org-agenda-refresh-files "_org")


;;;; Files

(defvar my/calendar-agenda-directory
  (file-name-as-directory (or (bound-and-true-p org-directory) "~/agenda"))
  "Directory of the diary files (`org-directory' if set, else ~/agenda).")

;; The plain diary file is no longer used, but keep its location in
;; the agenda directory.  Calendar marks its entries only if it exists.
(setq diary-file (expand-file-name "diary" my/calendar-agenda-directory)
      calendar-mark-diary-entries-flag (file-readable-p diary-file)
      diary-display-function #'diary-fancy-display)

;; Org-based diary: `i' in the agenda, the "d" capture template and
;; `i d' in the calendar all file entries into its date tree.
(setq org-agenda-diary-file
      (expand-file-name "diary.org" my/calendar-agenda-directory))

(defun my/calendar-ensure-org-diary-file ()
  "Create `org-agenda-diary-file' if missing and make it an agenda file.
The new file has a Holidays entry so that the holidays of
`calendar-holidays' show up in the Org agenda."
  (unless (file-exists-p org-agenda-diary-file)
    (make-directory (file-name-directory org-agenda-diary-file) t)
    (with-temp-file org-agenda-diary-file
      (insert "#+TITLE: Diary\n"
              "#+STARTUP: content\n\n"
              "* Holidays\n"
              "%%(org-calendar-holiday)\n\n"
              "* Anniversaries\n"
              "# e.g. %%(diary-anniversary 10 31 1990) Wedding (%d years)\n\n"))
    (when (fboundp 'my/org-agenda-refresh-files)
      (my/org-agenda-refresh-files))))

(my/calendar-ensure-org-diary-file)


;;;; Observed holidays

(defvar holiday-observed-default-rule '((6 . -1) (0 . 1))
  "Default rule for `holiday-observed-fixed'.
An alist of (DAY-OF-WEEK . OFFSET): a holiday falling on
DAY-OF-WEEK (0 = Sunday ... 6 = Saturday) is observed OFFSET days
later (earlier if negative).  The default moves Saturday to the
preceding Friday and Sunday to the following Monday.")

(defun holiday-observed-fixed (month day string &optional rule)
  "Observed holiday for the fixed date MONTH DAY, called STRING.

Like `holiday-fixed', but when the date falls on a day of the
week listed in RULE, the holiday is moved by the given offset and
\" (observed)\" is appended to STRING.  RULE defaults to
`holiday-observed-default-rule'; see there for its format.

The observed date may fall in a neighboring year (e.g. New Year's
Day on a Saturday is observed on December 31), so the years around
the displayed one are all considered."
  (let ((rule (or rule holiday-observed-default-rule))
        result)
    (dolist (year (number-sequence (1- displayed-year) (1+ displayed-year)))
      (let* ((fixed (list month day year))
             (offset (alist-get (calendar-day-of-week fixed) rule 0)))
        (push (list (calendar-gregorian-from-absolute
                     (+ (calendar-absolute-from-gregorian fixed) offset))
                    (if (zerop offset) string (concat string " (observed)")))
              result)))
    (holiday-filter-visible-calendar (nreverse result))))

;;;; Company holidays

(defvar holiday-company-holidays
  '((holiday-observed-fixed 1 1 "New Year's Day")
    (holiday-float 1 1 3 "Martin Luther King Jr. Day")   ; 3rd Monday
    (holiday-float 2 1 3 "Presidents' Day")              ; 3rd Monday
    ;; Friday between April 11 and 17 (the last Friday on or before
    ;; April 17).  Matches 2023-04-14, 2024-04-12, 2025-04-11 and
    ;; 2026-04-17.
    (holiday-float 4 5 -1 "Wellbeing Day" 17)
    (holiday-float 5 1 -1 "Memorial Day")                ; last Monday
    (holiday-observed-fixed 6 19 "Juneteenth Day")
    (holiday-observed-fixed 7 4 "Independence Day")
    (holiday-float 9 1 1 "Labor Day")                    ; 1st Monday
    (holiday-float 11 4 4 "Thanksgiving Day")            ; 4th Thursday
    (holiday-sexp '(calendar-gregorian-from-absolute
                    (1+ (calendar-absolute-from-gregorian
                         (calendar-nth-named-day 4 4 11 year))))
                  "Day After Thanksgiving")
    ;; Christmas Eve moves out of the way of an observed Christmas:
    ;; Fri -> Thu (Christmas on Sat is observed on Fri),
    ;; Sat -> Fri, Sun -> Fri (Christmas on Mon).
    (holiday-observed-fixed 12 24 "Christmas Eve" '((5 . -1) (6 . -1) (0 . -2)))
    (holiday-observed-fixed 12 25 "Christmas Day"))
  "Official company holidays, in the format of `calendar-holidays'.")


;;;; Korean (HQ) holidays

(use-package korean-holidays
  :ensure t
  :defer t)

(defvar holiday-hq-holidays
  '((holiday-fixed 11 1 "삼성전자 창립일"))
  "HQ holidays in Korea that are not in `korean-holidays'.")

(defvar my/holiday-korean-prefix "[KR] "
  "Prefix of the names of Korean holidays.")

(defun my/holiday-korean ()
  "Visible Korean holidays, with names prefixed by `my/holiday-korean-prefix'.
They are `korean-holidays' and `holiday-hq-holidays'."
  (require 'korean-holidays nil t)
  (let ((calendar-holidays (append (bound-and-true-p korean-holidays)
                                   holiday-hq-holidays)))
    (mapcar (lambda (h) (list (car h) (concat my/holiday-korean-prefix (cadr h))))
            (calendar-holiday-list))))

(defconst my/holiday-korean-form '(my/holiday-korean)
  "The entry of `calendar-holidays' that adds the Korean holidays.")

(setq calendar-holidays
      (append holiday-company-holidays (list my/holiday-korean-form)))

(defface my/calendar-korean-holiday
  '((((class color) (background light)) :background "light steel blue")
    (((class color) (background dark)) :background "dark slate blue")
    (t :inverse-video t))
  "Face for Korean holidays in the calendar."
  :group 'holidays)

(defun my/calendar-mark-holidays (orig &optional event)
  "Around advice for `calendar-mark-holidays' (ORIG, EVENT).
Mark official holidays with `calendar-holiday-marker' and the
remaining Korean holidays with `my/calendar-korean-holiday'."
  (let ((calendar-holidays (remove my/holiday-korean-form calendar-holidays))
        (korean (member my/holiday-korean-form calendar-holidays)))
    (funcall orig event)
    (when korean
      (with-current-buffer (if event
                               (window-buffer (posn-window (event-start event)))
                             (current-buffer))
        (let ((official (mapcar #'car (calendar-holiday-list))))
          (dolist (h (my/holiday-korean))
            (unless (member (car h) official)
              (calendar-mark-visible-date (car h) 'my/calendar-korean-holiday))))))))

(advice-add 'calendar-mark-holidays :around #'my/calendar-mark-holidays)


;;;; Diary sexps
;;;
;;; For %%(...) lines in Org files, e.g. in ~/agenda/diary.org:
;;;
;;;   %%(diary-nth-weekday t 1) First weekday of the month
;;;   %%(diary-nth-weekday t -1) Last weekday of the month
;;;   %%(diary-nth-weekday t -1 t) Last working day (skips company holidays)

(with-suppressed-warnings ((lexical date))
  (defvar date))                        ; bound by the diary or Org agenda

(defun my/diary-holiday-p (day holidays)
  "Return non-nil if DAY is one of HOLIDAYS.
HOLIDAYS is a list of holiday forms as in `calendar-holidays', or t
for `holiday-company-holidays'."
  (let ((calendar-holidays (if (eq holidays t) holiday-company-holidays holidays)))
    (calendar-check-holidays day)))

(defun diary-nth-weekday (month n &optional skip-holidays)
  "Diary sexp: the Nth weekday (Monday to Friday) of MONTH.

MONTH is a month number, a list of month numbers, or t for every
month, as in `diary-float'.  N is 1 for the first weekday of the
month, 2 for the second, and so on; -1 for the last, -2 for the
second to last, and so on.

If SKIP-HOLIDAYS is t, days in `holiday-company-holidays' are not
counted.  It can also be a list of holiday forms to skip, e.g.
`calendar-holidays' to also skip Korean holidays.

Examples, in an Org file:
  %%(diary-nth-weekday t 1) First weekday of every month
  %%(diary-nth-weekday t -1 t) Last working day of every month
  %%(diary-nth-weekday \\='(3 6 9 12) -1) Last weekday of each quarter"
  ;; `date' is the date being checked, bound dynamically by the
  ;; diary or the Org agenda.
  (let* ((cur date)
         (m (calendar-extract-month cur))
         (y (calendar-extract-year cur))
         days)
    (when (or (eq month t)
              (eql month m)
              (and (consp month) (memq m month)))
      (dotimes (i (calendar-last-day-of-month m y))
        (let ((day (list m (1+ i) y)))
          (when (and (memq (calendar-day-of-week day) '(1 2 3 4 5))
                     (not (and skip-holidays
                               (my/diary-holiday-p day skip-holidays))))
            (push day days))))
      ;; DAYS is now in reverse order.
      (equal cur (if (natnump n)
                      (nth (1- n) (nreverse days))
                    (nth (- -1 n) days))))))


;;;; Calendar

(defun my/calendar-org-capture-diary ()
  "Capture an Org diary entry for the date at point in the calendar."
  (interactive)
  (pcase-let ((`(,month ,day ,year) (calendar-cursor-to-date t)))
    (let ((org-overriding-default-time (encode-time 0 0 0 day month year)))
      (org-capture nil "d"))))

(use-package calendar
  :ensure nil
  :bind (("C-<f12>" . calendar))
  :custom
  (calendar-mark-holidays-flag t)       ; show holidays by default
  :config
  ;; The plain diary is not used: show the Org agenda instead.
  (keymap-set calendar-mode-map "d" #'org-calendar-goto-agenda)
  (keymap-set calendar-mode-map "i d" #'my/calendar-org-capture-diary))


;;;; Org capture helper

(defun my/org-capture-diary-timestamp ()
  "Return an active timestamp for the date of the current capture.
The time of day is included only when one was given."
  (let* ((time (or (org-capture-get :default-time) (current-time)))
         (d (decode-time time))
         (hm (list (nth 2 d) (nth 1 d))))
    (format-time-string
     (org-time-stamp-format
      (not (member hm (list '(0 0)
                            (list (bound-and-true-p org-extend-today-until) 0)))))
     time)))
