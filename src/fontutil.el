;;; fontutil-v2.el --- Latin/Hangul aligned font presets  -*- lexical-binding: t; -*-

;; Requires Emacs 29+ (`keymap-global-set', `string-pixel-width').
;;
;; Goal: one Hangul glyph is exactly two Latin glyphs wide, so that
;; org tables, ASCII art, etc. stay aligned in mixed English/Korean text.
;;
;; Quick start:
;;   (require 'fontutil)
;;   (fontutil/install-mouse-wheel)
;;   (fontutil/set-font 'scodepro-16)     ; or M-x fontutil/set-font
;;
;; Daemon users: the font list is computed lazily on the first graphic
;; frame, so call `fontutil/set-font' from a frame hook, e.g.
;;   (add-hook 'server-after-make-frame-hook
;;             (lambda () (when (display-graphic-p)
;;                          (fontutil/set-font 'scodepro-16))))
;;
;; Diagnostics:
;;   M-x fontutil/check-alignment       current font
;;   M-x fontutil/check-all             every available preset -> report buffer
;;   M-x fontutil/suggest-hangul-size   measured Hangul size for current preset
;;   M-x fontutil/generate-family       measure a NEW font set (Latin, Hangul,
;;                                      optional punctuation and symbol
;;                                      fonts) and print a family entry
;;
;; ---------------------------------------------------------------------
;; FONT BACKGROUND (read this once)
;; ---------------------------------------------------------------------
;; * Cell: the editor is a grid.  The cell width is the pixel advance of
;;   the default (Latin) font, i.e. `(default-font-width)'.
;; * `:size N' (integer) in a font-spec is PIXELS (em-square height).
;;   A float would be points.  Face `:height' is 1/10 point.
;; * Advance: pixel width a glyph is drawn with = design width * size,
;;   rounded to whole pixels.  Source Code Pro Latin is ~0.6em, so size 16
;;   gives 9.6 -> 10px cell.
;; * `char-width': how many cells Emacs ASSUMES a character takes (1 or 2).
;;   It is only a promise; the font may draw something different.
;; * Alignment: drawn pixel width == `char-width' * cell for every char.
;; * Hangul is ~1em wide, so its size should be 2 x cell width
;;   (Latin 16 -> cell 10px -> Hangul 20).
;; * Fontset: lookup table "which font draws which characters".  The
;;   default font is tried first; for characters it lacks, Emacs asks the
;;   fontset (`set-fontset-font'), keyed by script or by code range.
;;
;; ---------------------------------------------------------------------
;; TROUBLESHOOTING
;; ---------------------------------------------------------------------
;; Symptom: Korean text does not take exactly two Latin cells.
;;   -> M-x fontutil/check-alignment shows `한=NNpx(want MM)'.
;;   -> M-x fontutil/suggest-hangul-size, put the number in the table
;;      (`fontutil/families'), or use `auto' for that size.
;;
;; Symptom: only 字 (Hanja) is off, e.g. `字=15px(want 16)'.
;;   -> Hangul is fine but the font drawing Hanja differs.  Run
;;      C-u C-x = on a Hanja character and read the `font:' line.
;;   -> Hanja is fitted separately from Hangul (`fontutil/fit-hanja-size').
;;      It needs a font that really has Hanja: set `:hanja' in the family
;;      or edit `fontutil/hanja-families' (Noto Sans Mono CJK KR is the
;;      default).  A message "no aligned Hanja size" means that font lacks
;;      Hanja glyphs or is not installed.
;;
;; Symptom: only punctuation / box drawing is off (“ ” … · ─ │).
;;   -> The wrong font is drawing them.  Put the cursor on the character
;;      and run C-u C-x = (`describe-char'); look at the `font:' line.
;;   -> Option 1 (enabled by default): `fontutil/pin-latin-ranges' forces
;;      the Latin font.  If the character is STILL wrong, the Latin font
;;      probably lacks that glyph: choose another Latin font, or try
;;      Option 2 below.
;;   -> Option 2 (commented out): declare those characters 2 cells wide.
;;
;; Symptom: checker says `“=11px(want 22)', i.e. drawn width is exactly
;;   ONE cell but `want' is TWO.
;;   -> The font is fine; Emacs's `char-width' wrongly says 2.  Typical
;;      cause: these are East-Asian-Ambiguous characters and a Korean/CJK
;;      language environment (or `cjk-ambiguous-chars-are-wide') makes
;;      Emacs count them double.  Verify with (char-width ?“) -> 2.
;;   -> Fixed by `fontutil/narrow-latin-ranges' (called automatically
;;      with Option 1).  Re-run M-x fontutil/set-font, then re-align
;;      existing org tables with C-c C-c.
;;   -> If you instead WANT them double width, use Option 2 and make sure
;;      the font really draws two cells.
;;
;; Symptom: "Font preset `foo-16' is not available".
;;   -> The Latin or Hangul family is not installed (or you are in a
;;      non-graphic frame / daemon without a GUI frame yet).  Run
;;      M-x fontutil/reload in a GUI frame; set `fontutil/verbose' to t to
;;      see why a family was skipped.  Check names with
;;      (dolist (f (font-family-list)) (princ (format "%s\n" f))).
;;
;; Symptom: `fontutil/set-font' changes nothing visible.
;;   -> Make sure it runs in a graphic frame (daemon: use the frame hook
;;      above).  Existing text sometimes needs a redisplay; try making a
;;      new frame.
;;
;; Symptom: Symbola (or another symbol font) is ignored.
;;   -> Set `fontutil/prefer-symbol-font' to t (Emacs 28+).

(require 'seq)
(require 'popup-buffer nil 'noerror)

;;; ------------------------------------------------------------------
;;; Variables
;;; ------------------------------------------------------------------

(defvar fontutil/set-font-hook '(fontutil/set-fixed-pitch-serif)
  "Functions called with the preset name after `fontutil/set-font'.")

(defvar fontutil/must-set-fixed-pitch-serif 'undefined
  "If non-nil, `fontutil/set-fixed-pitch-serif' will proceed.")

(defvar fontutil/verbose nil
  "If non-nil, report presets dropped because a font is missing.")

(defvar fontutil/prefer-symbol-font nil
  "If non-nil, set `use-default-font-for-symbols' to nil on apply.
Emacs 28+ tries the default font for symbols before the fontset's
`symbol' entry.  Enable this if your symbol font is being ignored.")

(defvar fontutil/hangul-scripts '(hangul)
  "Scripts mapped to the Hangul font, sized so 한 is two cells wide.")

(defvar fontutil/hanja-scripts '(han cjk-misc)
  "Scripts mapped to the Hanja font, sized so 字 is two cells wide.
`han' covers Hanja and `cjk-misc' covers punctuation such as the corner
brackets.  Add `kana' if you want that too.  These are sized separately
from Hangul because one font often draws Hanja and Hangul at different
widths for the same size (Noto Sans Mono CJK KR does).")

(defvar fontutil/hanja-families '("Noto Sans Mono CJK KR" "NotoSansMonoCJKkr")
  "Default candidate families for Hanja when a family has no `:hanja'.
NanumGothicCoding appears to lack Hanja glyphs, so Emacs falls back to
an arbitrary font at an arbitrary width.  If none of these is installed,
the family's own Hangul font is used instead.")

(defvar fontutil/check-chars (string-to-list "“”‘’…·•─│")
  "Characters that commonly break alignment, tested by the checkers.")

;; --- Option 1 (active): pin ambiguous characters to the Latin font -------
;;
;; Emacs treats these as 1 cell, but a fallback Korean (or symbol) font
;; often draws them 2 cells wide, which breaks tables and ASCII art.
;; Mapping the ranges to the family's `:punct' font (or the Latin font if it
;; has none) REPLACES whatever entry they had.  Replacing was needed:
;; prepending lost to the Korean charset's entry for U+00B7.  A glyph the
;; font lacks is still drawn via Emacs's generic fallback, perhaps in an
;; arbitrary font, so choose a `:punct' font that has these characters.
(defvar fontutil/pin-latin t
  "If non-nil, draw `fontutil/latin-ranges' with the Latin font (Option 1).
Set to nil if you switch to Option 2 below.")

(defvar fontutil/latin-ranges
  '((#x00B7 . #x00B7)                   ; · middle dot
    (#x2018 . #x201F)                   ; ‘ ’ “ ”
    (#x2020 . #x2027)                   ; † ‡ • … (U+2026 is the ellipsis)
    (#x2500 . #x257F))                  ; box drawing ─ │ ┌ ┐ ...
  "Code ranges drawn by the Latin font when `fontutil/pin-latin' is non-nil.
Add (#x2190 . #x21FF) for arrows, etc.  Find a character's code with
`describe-char' (C-u C-x =).")

;; --- Option 2 (commented out): declare them 2 cells wide ------------------
;;
;; Use this instead if you prefer Korean-style full-width quotes and
;; ellipsis.  Emacs's promise (`char-width') is changed to match the
;; 2-cell glyphs the Korean font draws.  To try it:
;;   1. set `fontutil/pin-latin' to nil above,
;;   2. uncomment the block below,
;;   3. call (fontutil/widen-ambiguous) once (e.g. from init.el), then
;;      re-apply your font with `fontutil/set-font'.
;; Caveat: `char-width-table' is global; it changes column counting in
;; every buffer, so keep the ranges small.  Emacs may also have the
;; variable `cjk-ambiguous-chars-are-wide' (check with C-h v) which
;; widens a whole class of characters at once.
;;
;; (defvar fontutil/wide-ranges
;;   '((#x2018 . #x201F)                 ; ‘ ’ “ ”
;;     (#x2026 . #x2026))                ; …
;;   "Ranges declared 2 cells wide by `fontutil/widen-ambiguous'.")
;;
;; (defun fontutil/widen-ambiguous ()
;;   "Make `char-width' report 2 for `fontutil/wide-ranges'."
;;   (dolist (range fontutil/wide-ranges)
;;     (set-char-table-range char-width-table range 2)))
;;
;; With Option 2, `fontutil/check-alignment' will report these characters
;; as OK only if the Korean font really draws them 2 cells wide; if it
;; still reports a mismatch, the fallback font is not the one you think
;; (use `describe-char') or its glyph is not exactly 2 cells.

(defvar fontutil/current nil
  "Name of the preset most recently applied by `fontutil/set-font'.")

(defvar fontutil--presets nil
  "Cache of resolved presets; see `fontutil/presets'.")

;;; ------------------------------------------------------------------
;;; Font table
;;; ------------------------------------------------------------------

(defvar fontutil/families
  '((scodepro :latin ("SourceCodePro" "Source_Code_Pro" "Source Code Pro")
              :weight normal
              :hangul ("NanumGothicCoding") :symbol ("Symbola")
              :sizes ((14 16 17) (15 18 20) (16 20 22) (18 22 24) (20 24 24)))
    (ubuntu :latin ("UbuntuMono") :hangul ("NanumGothicCoding")
            :symbol ("Symbola") :spacing 4
            :sizes ((16 16 17) (18 18 20) (20 20 22) (22 22 22)))
    (dejavu :latin ("DejaVuSansMono") :hangul ("NanumGothicCoding")
            :symbol ("Symbola") :spacing 4
            :sizes ((16 16 17) (18 18 20) (20 20 22)))
    (nova :latin ("NovaMono") :hangul ("NanumGothicCoding")
          :symbol ("Symbola")
          :sizes ((16 18 20) (18 20 22)))
    (inconsolata :latin ("Inconsolata") :hangul ("NanumGothicCoding")
                 :symbol ("Symbola")
                 :sizes ((14 14 15 2) (15 16 17 2) (16 16 17 3) (20 20 22 3)))
    (consolas :latin ("Consolas") :hangul ("NanumGothicCoding")
              :symbol ("Symbola")
              :sizes ((14 16 16 2) (15 16 16 2) (16 18 17 3) (20 22 25 3)))
    (monaco :latin ("Monaco") :hangul ("NanumGothicCoding")
            :symbol ("Symbola")
            :sizes ((14 16 17) (15 18 20) (16 22 22)))
    (menlo :latin ("Menlo") :hangul ("NanumGothicCoding")
           :symbol ("Symbola")
           :sizes ((14 16 20) (16 20 20) (18 22 20)))
    (pt :latin ("PT Mono") :hangul ("NanumGothicCoding")
        :symbol ("Symbola")
        :sizes ((14 16 20 4) (16 20 20) (18 22 20 3)))
    (hack :latin ("Hack") :hangul ("NanumGothicCoding")
          :symbol ("Symbola")
          :sizes ((14 16 20) (16 20 20) (18 22 20)))
    ;; Second names are the WSL-style spellings.  The symbol font is
    ;; optional: presets still work when it is missing.
    (noto :latin ("Noto Sans Mono" "NotoSansMono") :weight normal
          :hangul ("Noto Sans Mono CJK KR" "NotoSansMonoCJKkr")
          :symbol ("Noto Sans Symbols")
          :sizes ((14 17 16) (16 22 16) (18 24 18) (20 26 18)))
    (cascadia :latin ("CascadiaCode") :hangul ("NanumGothicCoding")
              :symbol ("Symbola")
              :sizes ((14 16 20) (16 18 20) (18 22 20) (20 24 20)))
    (firacode :latin ("Fira Code") :hangul ("NanumGothicCoding")
              :symbol ("Symbola")
              :sizes ((14 16 20) (15 18 20) (16 20 15) (18 22 24)))
    (adwaita :latin ("Adwaita Mono") :hangul ("NanumGothicCoding")
             :symbol ("Symbola")
             :sizes ((14 16 20) (16 20 20) (18 22 20) (20 24 20)))
    ;; Comic Mono lacks the punctuation/box-drawing glyphs, so they come
    ;; from a separate monospaced font (:punct); the first installed wins.
    (ComicMono :latin ("Comic Mono") :hangul ("NanumGothicCoding")
               :punct ("DejaVu Sans Mono" "Noto Sans Mono" "Hack"
                       "Source Code Pro")
               :sizes ((14 16 nil) (16 18 nil) (18 20 nil) (20 22 nil)))
    (comic :latin ("Comic Mono") :hangul ("NanumGothicCoding")
	   :punct ("SauceCodePro Nerd Font Mono")
	   :symbol ("Symbols Nerd Font Mono")
	   :sizes ((14 16 auto) (16 18 auto) (18 20 auto) (20 22 auto)
		   (22 24 auto) (24 26 auto) (26 28 auto) (28 30 auto)
		   (30 32 auto)))
    ;; ProFont's advance is narrower than 0.6em, so the Hangul size is
    ;; measured at apply time (`auto') instead of being hard-coded.
    ;; Once `fontutil/suggest-hangul-size' gives you the numbers you can
    ;; replace `auto' with them.
    (profont :latin ("ProFont IIx Nerd Font Mono")
             :hangul ("NanumGothicCoding") :symbol ("Symbola")
             :sizes ((14 auto 20) (16 auto 20) (18 auto 20) (20 auto 20))))
  "Font families and their size presets.

Each element is (ID . PLIST) with these keys:
  :latin   candidate Latin family names; the first installed one is used
  :hangul  candidate Hangul family names (required)
  :symbol  candidate symbol family names (optional; none = no symbol
           mapping, Emacs picks its own fallback)
  :punct   candidate family names for `fontutil/latin-ranges' (curly
           quotes, ellipsis, box drawing...; optional; none = the Latin
           font).  Its size is measured when the preset is applied so
           that one glyph is exactly one cell wide.
  :hanja   candidate Hanja family names (optional; default
           `fontutil/hanja-families', else the Hangul font)
  :weight  optional weight for the Latin font
  :spacing default `line-spacing' frame parameter
  :sizes   list of (LATIN HANGUL SYMBOL [SPACING]), one per preset

The preset name is ID-LATIN, e.g. `scodepro-16'.  HANGUL and SYMBOL may
be the symbol `auto' to measure them when the preset is applied (Hangul:
한 two cells wide; symbol: → one cell wide).  SYMBOL may be nil to skip.

Integer sizes are in pixels.  Emacs rounds the Latin advance to whole
pixels, and a Hangul glyph is about 1em wide, so the Hangul size should
be 2 x (Latin advance in px).  Source Code Pro is 0.6em: size 16 gives
10px, so Hangul size 20.")

;;; ------------------------------------------------------------------
;;; Resolving presets (lazy, so daemon mode works)
;;; ------------------------------------------------------------------

(defun fontutil/first-available (families)
  "Return the first of FAMILIES that is installed, or nil."
  (seq-find (lambda (f) (find-font (font-spec :family f))) families))

(defun fontutil/expand-family (id spec)
  "Turn family ID with SPEC into a list of resolved presets."
  (let* ((latin (fontutil/first-available (plist-get spec :latin)))
        (hangul (fontutil/first-available (plist-get spec :hangul)))
        (symbol (fontutil/first-available (plist-get spec :symbol)))
        (weight (plist-get spec :weight))
        (punct (fontutil/first-available (plist-get spec :punct)))
        ;; Hanja font: the family's own :hanja, else the global defaults,
        ;; else the Hangul font itself.
        (hanja (or (fontutil/first-available
                    (or (plist-get spec :hanja) fontutil/hanja-families))
                   hangul)))
    (cond
     ((not (and latin hangul))
      (when fontutil/verbose
        (message "fontutil: skipping %s (latin=%S hangul=%S)"
                 id latin hangul))
      nil)
     (t
      (mapcar
       (lambda (s)
         (pcase-let ((`(,size ,hsize ,ssize ,spacing) s))
           (list :name (intern (format "%s-%d" id size))
                 :family id
                 :size size
                 :latin (append (list :family latin :size size)
                                (when weight (list :weight weight)))
                 :hangul (list :family hangul :size hsize)
                 :hanja (list :family hanja)
                 :punct (when punct (list :family punct))
                 :symbol (when (and symbol ssize)
                           (list :family symbol :size ssize))
                 :spacing (or spacing (plist-get spec :spacing)))))
       (plist-get spec :sizes))))))

(defun fontutil/presets ()
  "Return the list of presets whose fonts are installed.
Computed on first use in a graphic frame, then cached.  Returns nil
on a non-graphic display (and does not cache)."
  (or fontutil--presets
      (when (display-graphic-p)
        (setq fontutil--presets
              (mapcan (lambda (f) (fontutil/expand-family (car f) (cdr f)))
                      fontutil/families)))))

;;;###autoload
(defun fontutil/reload ()
  "Forget cached presets and re-detect installed fonts."
  (interactive)
  (setq fontutil--presets nil)
  (message "fontutil: %d presets available" (length (fontutil/presets))))

(defun fontutil/preset (name)
  "Return the preset named NAME (a symbol), or nil."
  (seq-find (lambda (p) (eq (plist-get p :name) name)) (fontutil/presets)))

(defun fontutil/display-presets ()
  "Pop up buffer containing all `fontutil/preset'."
  (interactive)
  (with-popup-buffer ("*fontutil/presets*" :mode #'emacs-lisp-mode
		      :action '((display-buffer-at-bottom)
				(dedicated . t)
				(window-height . fit-window-to-buffer)))
    (dolist (i (fontutil/presets))
      (insert (prin1-to-string i) "\n"))))

;;; ------------------------------------------------------------------
;;; Measuring
;;; ------------------------------------------------------------------

(defun fontutil/map-scripts (scripts family size)
  "Map each of SCRIPTS to FAMILY at pixel SIZE in the default fontset."
  (dolist (script scripts)
    (set-fontset-font t script (font-spec :family family :size size))))

(defun fontutil/set-hangul-font (family size)
  "Map `fontutil/hangul-scripts' to FAMILY at pixel SIZE."
  (fontutil/map-scripts fontutil/hangul-scripts family size))

(defun fontutil/set-hanja-font (family size)
  "Map `fontutil/hanja-scripts' to FAMILY at pixel SIZE."
  (fontutil/map-scripts fontutil/hanja-scripts family size))

(defun fontutil/pin-latin-ranges (family size)
  "Use FAMILY at pixel SIZE for `fontutil/latin-ranges' (Option 1).
Replaces any existing fontset entry for those ranges.  A plain
replacement is needed because prepending lost to the Korean charset's
entry for U+00B7, which Emacs drew with a proportional fallback font."
  (dolist (range fontutil/latin-ranges)
    (set-fontset-font t range (font-spec :family family :size size))))

(defun fontutil/narrow-latin-ranges ()
  "Tell Emacs `fontutil/latin-ranges' are ONE cell wide (`char-width' = 1).
Companion to `fontutil/pin-latin-ranges': once the Latin font draws these
characters one cell wide, Emacs's own width table must agree.  Otherwise
the checker reports `want 2 cells' and org tables (which pad using
`string-width') come out one cell too narrow around these characters.
`char-width-table' is global, so this affects every buffer; the ranges are
small on purpose."
  (dolist (range fontutil/latin-ranges)
    (set-char-table-range char-width-table range 1)))

(defun fontutil/fit-size (scripts family char)
  "Return the smallest pixel size at which CHAR is two cells wide.
SCRIPTS are mapped to FAMILY at each trial size and CHAR is measured
against the current default font's cell width.  Returns nil if no size
fits, e.g. when FAMILY lacks CHAR, so Emacs measures some fallback font
whose width never changes.  Leaves the last tried size installed in the
fontset, so callers should set the real one afterwards."
  (let* ((target (* 2 (default-font-width)))
         (lo (max 6 (- target 6)))
         (hi (+ target 8)))
    (catch 'fit
      (dolist (size (number-sequence lo hi))
        (fontutil/map-scripts scripts family size)
        (clear-font-cache)
        (when (= (string-pixel-width (string char)) target)
          (throw 'fit size))))))

(defun fontutil/fit-hangul-size (family)
  "Return the pixel size at which FAMILY's 한 is two cells wide, or nil."
  (fontutil/fit-size fontutil/hangul-scripts family ?한))

(defun fontutil/fit-hanja-size (family)
  "Return the pixel size at which FAMILY's 字 is two cells wide, or nil."
  (fontutil/fit-size fontutil/hanja-scripts family ?字))

(defun fontutil/fit-cell-size (family latin char)
  "Return the pixel size at which FAMILY draws CHAR exactly one cell wide.
LATIN is the Latin font plist (:family :size ...).  The cell width is read
from the current default font, which must already be LATIN.  FAMILY is
tried as the default face at each size, then the default face is
restored to LATIN.  Returns nil if no size fits (for instance when FAMILY
lacks CHAR so Emacs measures a fallback font)."
  (let ((cell (default-font-width))
        ;; Avoid resizing the frame while the default font changes.
        (frame-inhibit-implied-resize t))
    (unwind-protect
        (catch 'fit
          (dolist (size (number-sequence (max 6 (- cell 2)) (+ (* 2 cell) 4)))
            (set-face-attribute 'default nil
                                :font (font-spec :family family :size size))
            (clear-font-cache)
            (when (= (string-pixel-width (string char)) cell)
              (throw 'fit size))))
      (set-face-attribute 'default nil :font (apply #'font-spec latin)))))

(defun fontutil/fit-punct-size (family latin)
  "Return the size at which FAMILY's ─ is one cell wide, or nil."
  (fontutil/fit-cell-size family latin ?─))

(defun fontutil/fit-symbol-size (family latin)
  "Return the size at which FAMILY's → is one cell wide, or nil.
Arrows are the usual symbols and have a `char-width' of 1."
  (fontutil/fit-cell-size family latin ?→))

(defun fontutil/alignment-report ()
  "Return \"OK\" or a description of characters that are not cell aligned.
Each character should be as wide as `char-width' says, in cells."
  (let* ((cell (default-font-width))
         (bad (delq nil
                    (mapcar
                     (lambda (ch)
                       (let ((w (string-pixel-width (string ch)))
                             (want (* cell (char-width ch))))
                         (unless (= w want)
                           (format "%c=%dpx(want %d)" ch w want))))
                     (append "a한字" fontutil/check-chars nil)))))
    (format "cell=%dpx %s" cell
            (if bad (string-join bad " ") "OK"))))

;;;###autoload
(defun fontutil/check-alignment ()
  "Report whether Hangul, Hanja and ambiguous-width characters align."
  (interactive)
  (message "%s" (fontutil/alignment-report)))

;;;###autoload
(defun fontutil/check-all ()
  "Apply each available preset and list the alignment results."
  (interactive)
  (let ((orig fontutil/current)
        (lines nil))
    (dolist (p (fontutil/presets))
      (fontutil/apply p)
      (push (format "%-16s %s" (plist-get p :name) (fontutil/alignment-report))
            lines))
    (when orig (fontutil/set-font orig))
    (with-current-buffer (get-buffer-create "*fontutil-report*")
      (erase-buffer)
      (insert (string-join (nreverse lines) "\n") "\n")
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun fontutil/generate-family (id latin hangul punct symbol sizes)
  "Measure sizes for a new font set and show a family entry to paste.
ID is the family id (a string, e.g. \"mymono\").  LATIN and HANGUL are
installed family names.  PUNCT and SYMBOL are family names or nil/\"\":
  PUNCT  font for curly quotes, ellipsis, box drawing and similar.  nil
         means the Latin font draws them, so no `:punct' is emitted.
  SYMBOL font for arrows and other symbols.  nil means no symbol
         mapping (the entry gets nil sizes and no `:symbol').
SIZES is a list of Latin PIXEL sizes (the unit of `:size' in
`fontutil/families', not points).

For each size the Latin font is applied, its cell width is read, and
these are searched for:
  Hangul  size at which 한 is exactly two cells wide.
  symbol  size at which → is exactly one cell wide.
A size where nothing fits gets `auto', which retries when the preset is
applied; if that also fails, that font has unusual metrics or lacks the
glyph, so use another font or Latin size.  The PUNCT size is not put in
the table because `fontutil/apply' measures it each time (via ─).  A
PUNCT font that cannot be fitted at some size is reported in a comment.
The result goes to the *fontutil-family* buffer; the previously selected
preset is restored afterwards."
  (interactive
   (let ((fams (delete-dups (font-family-list))))
     (list (read-string "Family id (e.g. mymono): ")
           (completing-read "Latin family: " fams nil t)
           (completing-read "Hangul family: " fams nil t)
           (completing-read "Punctuation family (empty = use Latin): "
                            fams nil t)
           (completing-read "Symbol family (empty = none): " fams nil t)
           (mapcar #'string-to-number
                   (split-string
                    (read-string "Latin pixel sizes (space separated): "
                                 "14 16 18 20"))))))
  (unless (display-graphic-p)
    (user-error "Run this in a graphic frame"))
  (when (equal punct "") (setq punct nil))
  (when (equal symbol "") (setq symbol nil))
  (dolist (f (delq nil (list latin hangul punct symbol)))
    (unless (find-font (font-spec :family f))
      (user-error "Font family %S not found" f)))
  (let ((orig fontutil/current)
        (rows nil))
    (unwind-protect
        (dolist (size sizes)
          (let ((lplist (list :family latin :size size)))
            (set-face-attribute 'default nil :font (apply #'font-spec lplist))
            (clear-font-cache)
            ;; Each fit leaves the default face on LATIN, so the cell width
            ;; read below is always the Latin one.
            (push (list size
                        (default-font-width)
                        (fontutil/fit-hangul-size hangul)
                        (and symbol (fontutil/fit-symbol-size symbol lplist))
                        (and punct (fontutil/fit-punct-size punct lplist)))
                  rows)))
      ;; The searches leave trial sizes installed; put the real font back.
      (when orig (fontutil/set-font orig)))
    (with-current-buffer (get-buffer-create "*fontutil-family*")
      (erase-buffer)
      (insert (format "(%s :latin (%S) :hangul (%S)\n" id latin hangul))
      (when punct (insert (format " :punct (%S)\n" punct)))
      (when symbol (insert (format " :symbol (%S)\n" symbol)))
      (insert " :sizes (\n")
      (dolist (r (nreverse rows))
        (pcase-let ((`(,size ,cell ,hsize ,ssize ,psize) r))
          (insert (format "   (%d %s %s)   ; cell %dpx%s%s%s\n"
                          size (or hsize 'auto)
                          (cond ((null symbol) 'nil) (ssize) (t 'auto))
                          cell
                          (if hsize "" ", hangul: no fit, using auto")
                          (if (or (null symbol) ssize) ""
                            ", symbol: no fit, using auto")
                          (if (or (null punct) psize) ""
                            ", punct: no fit, pick another font")))))
      (insert "   ))\n")
      (emacs-lisp-mode)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))
    (unless orig
      (message "fontutil: no preset was selected, so the default font was not restored"))))

;;;###autoload
(defun fontutil/suggest-hangul-size ()
  "Measure the Hangul size that fits the current preset."
  (interactive)
  (let* ((p (or (fontutil/preset fontutil/current)
                (user-error "Select a font with `fontutil/set-font' first")))
         (family (plist-get (plist-get p :hangul) :family))
         (size (fontutil/fit-hangul-size family)))
    (fontutil/apply p)                  ; restore the real mapping
    (message "%s: Latin %d -> (hangul :family %S :size %s)"
             (plist-get p :name) (plist-get (plist-get p :latin) :size)
             family (or size "none found in 8..40"))))

;;; ------------------------------------------------------------------
;;; Applying
;;; ------------------------------------------------------------------

(defun fontutil/apply (preset)
  "Apply PRESET (a plist from `fontutil/presets')."
  (let* ((latin (plist-get preset :latin))
         (hangul (plist-get preset :hangul))
         (symbol (plist-get preset :symbol))
         (spacing (plist-get preset :spacing))
         (hsize (plist-get hangul :size)))
    (clear-font-cache)
    (set-face-attribute 'default nil :font (apply #'font-spec latin))
    (when symbol
      (when (and fontutil/prefer-symbol-font
                 (boundp 'use-default-font-for-symbols))
        (setq use-default-font-for-symbols nil))
      (let ((ssize (plist-get symbol :size)))
        (when (eq ssize 'auto)
          (setq ssize (fontutil/fit-symbol-size (plist-get symbol :family)
                                                latin))
          (unless ssize
            (message "fontutil: no one-cell symbol size for %s with %s"
                     (plist-get preset :name) (plist-get symbol :family))
            (setq ssize (plist-get latin :size))))
        (set-fontset-font t 'symbol
                          (font-spec :family (plist-get symbol :family)
                                     :size ssize))))
    (when (eq hsize 'auto)
      (setq hsize (fontutil/fit-hangul-size (plist-get hangul :family)))
      (unless hsize
        ;; No size in 8..40 made Hangul exactly two cells wide.  Likely
        ;; causes: the Hangul font's glyphs are not ~1em wide (poor
        ;; metadata), or the Latin cell is odd-sized.  Fallback is 2x the
        ;; cell; check with M-x fontutil/check-alignment and try another
        ;; Latin size or Hangul font.
        (message "fontutil: no aligned Hangul size found for %s"
                 (plist-get preset :name))
        (setq hsize (* 2 (default-font-width)))))
    (fontutil/set-hangul-font (plist-get hangul :family) hsize)
    ;; Hanja is fitted on its own, always by measurement (字 must be two
    ;; cells).  If nothing fits, the font probably lacks Hanja: set
    ;; `:hanja' for the family, or install a CJK mono font.  The mapping is
    ;; then left to Emacs's fallback, so 字 will be misaligned.
    (let* ((hanja (plist-get preset :hanja))
           (family (plist-get hanja :family))
           (size (and family (fontutil/fit-hanja-size family))))
      (if size
          (fontutil/set-hanja-font family size)
        (when family
          (message "fontutil: no aligned Hanja size for %s with %s"
                   (plist-get preset :name) family)
          ;; Replace the leftover trial size from the search with a sane
          ;; guess (2x cell).  Do NOT pass nil to `set-fontset-font': that
          ;; means "no font" and would blank these characters.
          (fontutil/set-hanja-font family (* 2 (default-font-width))))))
    ;; Option 1: punctuation / box drawing come from the :punct font if the
    ;; family has one (sized so a glyph is one cell wide), else the Latin
    ;; font.  Plain replacement, so order relative to the other mappings
    ;; no longer matters.
    (when fontutil/pin-latin
      (let* ((punct (plist-get preset :punct))
             (pfamily (or (plist-get punct :family) (plist-get latin :family)))
             (psize (if punct
                        (or (fontutil/fit-punct-size pfamily latin)
                            (progn
                              (message "fontutil: no one-cell size for %s with %s"
                                       (plist-get preset :name) pfamily)
                              (plist-get latin :size)))
                      (plist-get latin :size))))
        (fontutil/pin-latin-ranges pfamily psize))
      ;; Font now draws them 1 cell wide; make `char-width' say so too.
      (fontutil/narrow-latin-ranges))
    ;; Always set line-spacing so a previous preset's value does not linger,
    ;; and merge into `default-frame-alist' instead of replacing it.
    (setf (alist-get 'line-spacing default-frame-alist) spacing)
    (let ((params (list (cons 'line-spacing spacing))))
      (dolist (f (frame-list))
        (modify-frame-parameters f params)))))

;;;###autoload
(defun fontutil/set-font (name)
  "Apply the preset NAME (symbol or string) from `fontutil/presets'."
  (interactive
   (list (completing-read
          "font: "
          (mapcar (lambda (p) (symbol-name (plist-get p :name)))
                  (fontutil/presets))
          nil t)))
  (let* ((name (if (stringp name) (intern name) name))
         (preset (fontutil/preset name)))
    (unless preset
      (user-error "Font preset `%s' is not available" name))
    (fontutil/apply preset)
    (setq fontutil/current name)
    (run-hook-with-args 'fontutil/set-font-hook name)))

;;; ------------------------------------------------------------------
;;; Zoom: step through presets so Latin and Hangul stay in sync
;;; ------------------------------------------------------------------

(defun fontutil/zoom (delta)
  "Move DELTA steps through the sizes of the current font family."
  (let* ((cur (or (fontutil/preset fontutil/current)
                  (user-error "Select a font with `fontutil/set-font' first")))
         (sibs (sort (seq-filter
                      (lambda (p) (eq (plist-get p :family)
                                      (plist-get cur :family)))
                      (fontutil/presets))
                     (lambda (a b) (< (plist-get a :size) (plist-get b :size)))))
         (idx (seq-position sibs cur #'eq))
         (next (and idx (nth (+ idx delta) sibs)))
         (pos (and idx (+ idx delta))))
    (if (and next (>= pos 0))
        (progn (fontutil/set-font (plist-get next :name))
               (message "Font: %s" (plist-get next :name)))
      (message "Font: no %s preset for %s"
               (if (> delta 0) "larger" "smaller") (plist-get cur :family)))))

(defun fontutil/increase-default-height ()
  "Switch to the next larger preset of the current family."
  (interactive)
  (fontutil/zoom 1))

(defun fontutil/decrease-default-height ()
  "Switch to the next smaller preset of the current family."
  (interactive)
  (fontutil/zoom -1))

(defun fontutil/bind-wheel ()
  "Bind C-wheel up/down to the preset zoom commands, globally.
Both event spellings are bound: `mouse-4'/`mouse-5' (older X builds) and
`wheel-up'/`wheel-down' (macOS, and newer builds that use XInput2 or
pgtk).  Bind only the first pair and, on a newer build, Ctrl+wheel falls
through to Emacs's own `mouse-wheel-text-scale', which scales just ONE
BUFFER (via face remapping) and ignores the fontset, so Hangul and Latin
no longer match and other windows do not change."
  (dolist (b '(("C-<mouse-4>"   . fontutil/increase-default-height)
               ("C-<mouse-5>"   . fontutil/decrease-default-height)
               ("C-<wheel-up>"  . fontutil/increase-default-height)
               ("C-<wheel-down>" . fontutil/decrease-default-height)))
    (keymap-global-set (car b) (cdr b))))

(defun fontutil/install-mouse-wheel ()
  "Bind C-wheel up/down to preset zoom on graphic displays.
The bindings are made again once `mwheel' has loaded, because
`mouse-wheel-mode' installs its own text-scale bindings on the same keys
and whichever runs last wins.  To see what a key does, use C-h k and then
turn the wheel with Ctrl held."
  (when (or (display-graphic-p) (daemonp))
    (fontutil/bind-wheel)
    (with-eval-after-load 'mwheel
      (fontutil/bind-wheel))))

;;; ------------------------------------------------------------------
;;; fixed-pitch-serif workaround
;;; ------------------------------------------------------------------

(defun fontutil/set-fixed-pitch-serif (name)
  "Make `fixed-pitch-serif' use the Latin family of preset NAME if needed.
On some systems this face has no usable font, which makes Info
manuals look awkward."
  (let* ((preset (fontutil/preset name))
         (family (plist-get (plist-get preset :latin) :family)))
    (when (eq fontutil/must-set-fixed-pitch-serif 'undefined)
      (let* ((old (face-attribute 'fixed-pitch-serif :family nil t))
             (found (and (stringp old)
                         (find-font (font-spec :family old)))))
        (setq fontutil/must-set-fixed-pitch-serif (not found))))
    (when (and family (eq fontutil/must-set-fixed-pitch-serif t))
      (set-face-attribute 'fixed-pitch-serif nil :family family))))

(provide 'fontutil)
;;; fontutil-v2.el ends here

