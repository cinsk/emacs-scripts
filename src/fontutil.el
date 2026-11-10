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
;;   -> Hanja is only managed when `fontutil/enable-hanja' is non-nil
;;      (off by default; 字 is then not checked at all).
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

(defvar fontutil/enable-hanja nil
  "If non-nil, map and size a Hanja font when a preset is applied.
When nil (the default), Hanja is left alone: presets get no Hanja font,
`fontutil/apply' does not measure or map `fontutil/hanja-scripts' (Emacs
draws Hanja with its own fallback font, unaligned), and
`fontutil/check-alignment' does not test 字.
Presets are cached, so after changing this run `fontutil/reload' and
re-select the font.  A Hanja mapping made before turning this off stays
until Emacs restarts.")

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

;; --- Math: ranges drawn by the family's `:math' font ----------------------
;;
;; Math fonts (Noto Sans Math, STIX Two Math, Cambria Math) cover far more
;; math than coding fonts, but they are proportional typesetting fonts, so
;; only some of their glyphs are one cell wide.  They are therefore used to
;; fill gaps: with `use-default-font-for-symbols' (the default) the Latin
;; font still draws the math operators it has, and the `:math' font draws
;; the rest.  The math letters (U+1D400..) are the `mathematical' script,
;; not `symbol', so `:symbol' never reaches them; only `:math' does.
(defvar fontutil/math-ranges
  '((#x2200 . #x22FF)                   ; ∀ ∂ ∑ ≤ math operators
    (#x27C0 . #x27EF)                   ; ⟂ ⟨ ⟩ misc math symbols A
    (#x2980 . #x29FF)                   ; ⦃ ⦄ ⧺ misc math symbols B
    (#x2A00 . #x2AFF)                   ; ⨀ ⨁ ⩽ supplemental operators
    (#x1D400 . #x1D7FF))                ; 𝐀 𝑥 𝔸 𝟙 math letters and digits
  "Code ranges drawn by the family's `:math' font, when it has one.")

(defvar fontutil--math-mapped nil
  "Non-nil once a preset has mapped `fontutil/math-ranges' to a font.
A later preset without `:math' then resets those ranges (see
`fontutil/apply'), so the previous math font does not linger.")

(defvar fontutil--math-sizes nil
  "Cache for `fontutil/fit-math-size': alist of ((FAMILY . CELL) . SIZE).")

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
  :math    candidate family names for `fontutil/math-ranges' (math
           operators and math letters; optional).  Its size is measured
           when the preset is applied: the size at which the most math
           glyphs are one cell wide.  It fills gaps, as the Latin font
           still draws the math operators it has.  None = math operators
           fall back to the `:symbol' font, math letters to Emacs's own
           fallback.
  :hanja   candidate Hanja family names (optional; default
           `fontutil/hanja-families', else the Hangul font)
  :weight  optional weight for the Latin font
  :spacing default `line-spacing' frame parameter
  :sizes   list of (LATIN HANGUL SYMBOL [SPACING]), one per preset

The preset name is ID-LATIN, e.g. `scodepro-16'.  HANGUL and SYMBOL
each take a pixel size, `auto' or nil:

  HANGUL (2nd)
    integer  fixed pixel size, used as is; nothing is measured.
    `auto'   measured each time the preset is applied: the size at
             which 한 is exactly two cells wide.  If nothing fits, a
             message is shown and 2 x the cell width is used.
    nil      the Hangul font is mapped with no size (:size nil), so
             Emacs picks one, normally following the default font;
             Hangul is then usually not two cells wide.  Rarely wanted:
             use a number or `auto'.

  SYMBOL (3rd)
    integer  fixed pixel size, used as is; nothing is measured.
    `auto'   measured each time the preset is applied: the size at
             which → is exactly one cell wide.  If nothing fits, a
             message is shown and the Latin size is used.
    nil      no symbol mapping for that preset (`fontutil/expand-family'
             drops `:symbol'), so Emacs picks its own fallback for
             arrows, math etc.  Normal when the Latin font covers the
             symbols or the family has no `:symbol' font.

`auto' is for when the number is not known yet; it works but measures
on every apply.  Replace it with the number from
`fontutil/suggest-hangul-size' (Hangul) or `fontutil/generate-family'.
`fontutil/generate-family' follows the same rules: a Hangul size it
cannot fit is written as `auto' (so apply uses the 2 x cell fallback),
a symbol size it cannot fit as nil (`auto' would just fail again on
every apply).

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
        (math (fontutil/first-available (plist-get spec :math)))
        ;; Hanja font: the family's own :hanja, else the global defaults,
        ;; else the Hangul font itself.  Skipped (nil) unless
        ;; `fontutil/enable-hanja', which also saves the font lookups.
        (hanja (and fontutil/enable-hanja
                    (or (fontutil/first-available
                         (or (plist-get spec :hanja) fontutil/hanja-families))
                        hangul))))
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
                 :hanja (when hanja (list :family hanja))
                 :punct (when punct (list :family punct))
                 :math (when math (list :family math))
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
  (setq fontutil--presets nil
        fontutil--math-sizes nil)
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

(defun fontutil/map-ranges (ranges family size)
  "Map each code range of RANGES to FAMILY at pixel SIZE.
Replaces the fontset entry for those characters (no ADD argument)."
  (dolist (range ranges)
    (set-fontset-font t range (font-spec :family family :size size))))

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

;; Why the fitting functions below measure fonts directly
;; ------------------------------------------------------
;; They used to measure by changing the live frame: install a trial size
;; (`set-fontset-font', or `set-face-attribute' on the default face),
;; then ask the display engine with `string-pixel-width', repeated for
;; every candidate size.  Doing that while init.el/postinit.el runs
;; crashed Emacs 30 (NS) about two times in three with a NULL pointer in
;; `face_for_font': redisplay had not yet caught up with the font change
;; when the next measurement ran.  `clear-font-cache' between trials
;; made it worse.
;;
;; `fontutil/glyph-width' instead opens the font object and reads the
;; glyph metrics, so measuring never touches faces, fontsets or
;; redisplay.  It returns the same numbers `string-pixel-width' gives
;; once the font is installed (checked against it for Hangul, ─ and →).
;; One difference: when the font lacks the character it returns nil,
;; while `string-pixel-width' silently measured Emacs's fallback font
;; (the old code could "fit" a size by accident that way).

(defun fontutil/glyph-widths (family size chars)
  "Return the advance widths in pixels of CHARS in FAMILY at pixel SIZE.
CHARS is a list of characters; the result has one element per
character, nil where FAMILY lacks it (all nil if FAMILY is not
installed).  The font is opened once, so this is cheap even for a few
hundred characters.  Nothing is changed on any frame; see the comment
above."
  (let* ((entity (find-font (font-spec :family family :size size)))
         (font (and entity (open-font entity size))))
    (if (null font)
        (make-list (length chars) nil)
      ;; `font-get-glyphs' returns a vector with one element per
      ;; character: nil if the font has no glyph for it, else a vector
      ;; [FROM TO C CODE WIDTH LBEARING RBEARING ASCENT DESCENT ...].
      (mapcar (lambda (glyph) (and glyph (aref glyph 4))) ; WIDTH = advance
              (font-get-glyphs font 0 (length chars) (apply #'string chars))))))

(defun fontutil/glyph-width (family size char)
  "Return the advance width in pixels of CHAR in FAMILY at pixel SIZE.
Returns nil if FAMILY is not installed or lacks CHAR."
  (car (fontutil/glyph-widths family size (list char))))

(defun fontutil/cell-width (family size)
  "Return the cell width in pixels Emacs uses for FAMILY at pixel SIZE.
That is the advance of a space, which equals `default-font-width' once
FAMILY is the default font (checked for several monospaced fonts), so a
font can be measured without making it the default."
  (fontutil/glyph-width family size ?\s))

(defun fontutil/fit-size (scripts family char &optional cell)
  "Return the smallest pixel size at which CHAR is two cells wide.
FAMILY's CHAR is measured against CELL, the cell width in pixels
(default: the current default font's).  Returns nil if no size fits,
e.g. when FAMILY lacks CHAR.  The fontset is not changed; callers
install the size they choose.  SCRIPTS is no longer needed (trial
sizes are not installed any more) and is kept so the callers stay the
same."
  (ignore scripts)
  (let* ((target (* 2 (or cell (default-font-width))))
         (lo (max 6 (- target 6)))
         (hi (+ target 8)))
    (seq-find (lambda (size)
                (eql (fontutil/glyph-width family size char) target))
              (number-sequence lo hi))))

(defun fontutil/fit-hangul-size (family &optional cell)
  "Return the pixel size at which FAMILY's 한 is two cells wide, or nil.
CELL is the cell width in pixels (default: the current default font's)."
  (fontutil/fit-size fontutil/hangul-scripts family ?한 cell))

(defun fontutil/fit-hanja-size (family)
  "Return the pixel size at which FAMILY's 字 is two cells wide, or nil."
  (fontutil/fit-size fontutil/hanja-scripts family ?字))

(defun fontutil/cell-sizes (cell)
  "Return the pixel sizes tried when fitting a glyph to CELL pixels."
  (number-sequence (max 6 (- cell 2)) (+ (* 2 cell) 4)))

(defun fontutil/fit-cell-size (family latin char &optional cell)
  "Return the pixel size at which FAMILY draws CHAR exactly one cell wide.
CELL is the cell width in pixels; by default it is read from the
current default font, which must then already be the Latin font.
Returns nil if no size fits (for instance when FAMILY lacks CHAR).  The
default face is not changed.  LATIN (the Latin font plist) was used to
restore the default face after trying FAMILY in it; that is no longer
done, and LATIN is kept so the callers stay the same."
  (ignore latin)
  (let ((cell (or cell (default-font-width))))
    (seq-find (lambda (size)
                (eql (fontutil/glyph-width family size char) cell))
              (fontutil/cell-sizes cell))))

(defun fontutil/fit-punct-size (family latin &optional cell)
  "Return the size at which FAMILY's ─ is one cell wide, or nil.
CELL is as in `fontutil/fit-cell-size'."
  (fontutil/fit-cell-size family latin ?─ cell))

(defun fontutil/fit-symbol-size (family latin &optional cell)
  "Return the size at which FAMILY's → is one cell wide, or nil.
Arrows are the usual symbols and have a `char-width' of 1.  CELL is as
in `fontutil/fit-cell-size'."
  (fontutil/fit-cell-size family latin ?→ cell))

;;; ------------------------------------------------------------------
;;; Coverage: which font should draw punctuation and symbols
;;; ------------------------------------------------------------------
;;
;; Used by `fontutil/generate-family' to tell whether a new family needs
;; `:punct', `:symbol' and/or `:math', and which installed fonts could
;; fill in.
;; For each group in `fontutil/coverage-groups' a font is scored by
;;   HAVE     characters it has a glyph for, and
;;   ALIGNED  those whose advance is exactly (char-width CH) cells,
;; out of TOTAL characters in the group.  A group is fine when ALIGNED =
;; TOTAL for the Latin font; otherwise the group's ROLE says which
;; preset key would cover it, and other fonts are searched as candidates.

(defvar fontutil/coverage-groups
  '(("punctuation" :punct (#x00B7 (#x2018 . #x2027)))
    ("box drawing" :punct ((#x2500 . #x257F)))
    ("arrows"      :symbol ((#x2190 . #x21FF)))
    ("shapes"      :symbol ((#x25A0 . #x25FF)))
    ("dingbats"    :symbol ((#x2700 . #x27BF)))
    ("math ops"    :math ((#x2200 . #x22FF)))
    ("math misc"   :math ((#x27C0 . #x27EF) (#x2980 . #x2AFF)))
    ("math letters" :math ((#x1D400 . #x1D7FF))))
  "Character groups checked by `fontutil/generate-family'.
Each element is (NAME ROLE RANGES).  RANGES is a list of characters and
\(FROM . TO) code ranges.  ROLE says which preset key draws the group
when the Latin font cannot:
  `:punct'   `fontutil/pin-latin-ranges' REPLACES the Latin font with
             the `:punct' font for all these groups at once (keep them
             in step with `fontutil/latin-ranges').
  `:symbol'  characters of the `symbol' script; the `:symbol' font fills
             the gaps the Latin font leaves.
  `:math'    `fontutil/math-ranges' (keep in step); the `:math' font
             fills the gaps, else the `:symbol' font does.")

(defun fontutil/group-chars (ranges)
  "Return the list of characters in RANGES (characters and (FROM . TO))."
  (mapcan (lambda (r)
            (if (consp r) (number-sequence (car r) (cdr r)) (list r)))
          ranges))

(defun fontutil/role-group-chars (role ranges)
  "Return the characters of RANGES that a ROLE font would really draw.
For `:symbol' only characters of the `symbol' script are kept: the
others in those blocks (◽ ✅ ❌ ...) are `emoji' script, drawn by the
emoji font whatever `:symbol' is, and are 2 cells wide, so counting
them would make every font look misaligned.  `:punct' and `:math'
ranges are mapped by code range, so all their characters count.
Unassigned code points (gaps in the math letters block) are dropped
for every ROLE."
  (seq-filter (lambda (ch)
                (and (not (eq (get-char-code-property ch 'general-category)
                              'Cn))
                     (or (not (eq role :symbol))
                         (eq (aref char-script-table ch) 'symbol))))
              (fontutil/group-chars ranges)))

(defun fontutil/role-chars (role &optional groups)
  "Return all characters of the GROUPS with ROLE.
GROUPS defaults to `fontutil/coverage-groups'."
  (mapcan (lambda (g)
            (and (eq (nth 1 g) role)
                 (fontutil/role-group-chars role (nth 2 g))))
          (or groups fontutil/coverage-groups)))

(defvar fontutil/describe-groups
  '(("ASCII"        :latin ((#x21 . #x7E)))
    ("Latin-1"      :latin ((#xA1 . #xFF)))
    ("Latin Ext-A"  :latin ((#x100 . #x17F)))
    ("Hangul"       :hangul ((#xAC00 . #xD7A3)))
    ("Hangul jamo"  :hangul ((#x3131 . #x318E)))
    ("Hanja"        :hanja ((#x4E00 . #x9FFF))))
  "Extra groups shown by `fontutil/describe-coverage'.
Same format as `fontutil/coverage-groups', which is shown after these.
`:latin' groups want one cell per character, `:hangul' and `:hanja'
two (as `char-width' says).")

(defun fontutil/coverage (family size cell chars)
  "Score FAMILY at pixel SIZE on CHARS against a CELL-pixel cell.
Return (HAVE ALIGNED TOTAL); see the comment above this function."
  (let ((have 0) (aligned 0))
    (seq-mapn (lambda (ch w)
                (when w
                  (setq have (1+ have))
                  (when (= w (* cell (char-width ch)))
                    (setq aligned (1+ aligned)))))
              chars (fontutil/glyph-widths family size chars))
    (list have aligned (length chars))))

(defun fontutil/best-cell-size (family cell chars)
  "Return (SIZE HAVE ALIGNED TOTAL) for FAMILY's best size on CHARS.
The best size is the one with the most cell-aligned CHARS (the smallest
such size on ties), tried over `fontutil/cell-sizes'.  Returns nil when
no size aligns any of CHARS."
  (let ((best nil))
    (dolist (size (fontutil/cell-sizes cell))
      (let ((score (fontutil/coverage family size cell chars)))
        (when (> (nth 1 score) (if best (nth 2 best) 0))
          (setq best (cons size score)))))
    best))

(defun fontutil/math-chars ()
  "Return the assigned characters of `fontutil/math-ranges'."
  (fontutil/role-group-chars :math fontutil/math-ranges))

(defun fontutil/fit-math-size (family &optional cell)
  "Return the size at which FAMILY draws the most math glyphs one cell wide.
Math fonts are proportional, so unlike the other fits this does not ask
for every glyph to fit: it picks the size that aligns the most of
`fontutil/math-ranges' (see `fontutil/best-cell-size').  CELL is the
cell width in pixels (default: the current default font's).  Returns
nil if no glyph fits at any size.
Measuring ~1700 glyphs at each trial size takes ~70 ms, so results are
cached per FAMILY and CELL; `fontutil/reload' clears the cache."
  (let* ((cell (or cell (default-font-width)))
         (key (cons family cell))
         (hit (assoc key fontutil--math-sizes)))
    (if hit
        (cdr hit)
      (let ((size (car (fontutil/best-cell-size family cell
                                                (fontutil/math-chars)))))
        (push (cons key size) fontutil--math-sizes)
        size))))

(defun fontutil/candidate-families (chars exclude &optional limit)
  "Return up to LIMIT (default 3) installed families covering most of CHARS.
Families in EXCLUDE are skipped, and so are hidden system fonts (names
starting with \".\") and LastResort, which claims every character but
only draws placeholder boxes.  Ranked by how many of CHARS they have,
monospaced fonts first on ties.  Only `font-has-char-p' is used here,
which is cheap; sizes are checked by the caller for the few winners."
  (let (scored)
    (dolist (f (delete-dups (font-family-list)))
      (unless (or (member f exclude)
                  (string-prefix-p "." f)
                  (string-match-p "LastResort" f))
        (let* ((entity (find-font (font-spec :family f)))
               (n (if entity
                      (seq-count (lambda (ch) (font-has-char-p entity ch))
                                 chars)
                    0)))
          (when (> n 0)
            (push (list f n (and (find-font (font-spec :family f :spacing 100))
                                 t))
                  scored)))))
    (seq-take (sort scored
                    (lambda (a b)
                      (or (> (nth 1 a) (nth 1 b))
                          (and (= (nth 1 a) (nth 1 b)) (nth 2 a)
                               (not (nth 2 b))))))
              (or limit 3))))

(defun fontutil/format-score (score)
  "Format a (HAVE ALIGNED TOTAL) SCORE for a coverage report."
  (pcase-let ((`(,have ,aligned ,total) score))
    (if (= have aligned)
        (format "%d/%d" have total)
      (format "%d/%d (%d wrong width)" have total (- have aligned)))))

(defun fontutil/format-size (size latin-size)
  "Format a fitted pixel SIZE, flagging one well below LATIN-SIZE.
A font that only fits the cell at under 3/4 of the Latin size draws
visibly small glyphs next to the Latin text."
  (format "%dpx%s" size (if (< size (* 0.75 latin-size)) " (small)" "")))

(defun fontutil/try-line (chars cell size exclude &optional note)
  "Return a \";;     try: ...\" line suggesting fonts for CHARS.
CELL is the cell width and SIZE the Latin size, both in pixels; fonts
in EXCLUDE are skipped.  NOTE, if non-nil, is appended in parentheses.
A wider pool is taken by glyph count (cheap), each is measured at its
best size (costly), and the 3 that draw the most glyphs one cell wide
are shown: a font with every glyph at the wrong width is worse than
one with fewer that align."
  (let* ((scored
          (delq nil
                (mapcar (lambda (c)
                          (let ((best (fontutil/best-cell-size (car c) cell
                                                               chars)))
                            (and best (cons (car c) best))))
                        (fontutil/candidate-families chars exclude 8))))
         ;; Each element is (FAMILY SIZE HAVE ALIGNED TOTAL).
         (ranked (seq-take (sort scored (lambda (a b)
                                          (> (nth 3 a) (nth 3 b))))
                           3))
         (cands (mapcar (lambda (c)
                          (format "%s %s @%s" (car c)
                                  (fontutil/format-score (cddr c))
                                  (fontutil/format-size (nth 1 c) size)))
                        ranked)))
    (concat ";;     try"
            (if note (format " (%s)" note) "")
            ": "
            (if cands
                (string-join cands ", ")
              "no installed font fits one cell"))))

(defun fontutil/coverage-report (latin size punct symbol &optional math)
  "Return comment lines on how LATIN at pixel SIZE covers each group.
PUNCT, SYMBOL and MATH are the family names given for those roles, or
nil.  For each group of `fontutil/coverage-groups' the Latin font is
scored.  When it does not draw every character one cell wide, the font
that would fill the gaps (see the roles in `fontutil/coverage-groups')
is scored at its best size, and other installed fonts are suggested.

`:punct' is different: its font replaces the Latin font for ALL the
`:punct' groups.  So the given `:punct' font is shown for every such
group, flagged when it is worse than the Latin font it replaces, and
`:punct' candidates are scored on all those groups together."
  (let* ((cell (fontutil/cell-width latin size))
         ;; The `:punct' try line covers all `:punct' groups, so it is
         ;; computed once, on first need.
         (punct-try nil)
         ;; Built with `push' and reversed at the end, so the first line
         ;; goes last here.
         (lines (list ";;   (HAVE/TOTAL glyphs; `wrong width' = not the cells `char-width' wants)"
                      (format ";; Coverage at %dpx (cell %dpx), Latin = %s"
                              size cell latin))))
    (dolist (g fontutil/coverage-groups)
      (pcase-let* ((`(,name ,role ,ranges) g)
                   (chars (fontutil/role-group-chars role ranges))
                   (score (fontutil/coverage latin size cell chars))
                   (latin-ok (= (nth 1 score) (nth 2 score)))
                   ;; The font that would draw this group (or its gaps),
                   ;; and how to name it: math falls back to `:symbol'.
                   (given (pcase role
                            (:punct punct)
                            (:symbol symbol)
                            (:math (or math symbol))))
                   (label (if (and (eq role :math) (not math))
                              "via :symbol"
                            (format "your %s" role)))
                   ;; (SIZE HAVE ALIGNED TOTAL) for GIVEN, or nil.
                   (given-best (and given
                                    (fontutil/best-cell-size given cell chars))))
        (push (format ";;   %-12s Latin %s: %s" name
                      (fontutil/format-score score)
                      (cond (latin-ok "OK")
                            ((eq role :math) "needs :math (or :symbol)")
                            (t (format "needs %s" role))))
              lines)
        ;; The given font matters where it fills gaps, and always for
        ;; `:punct', which replaces the Latin font even where it is OK.
        (when (and given (or (not latin-ok) (eq role :punct)))
          (let ((worse (and (eq role :punct)
                            (< (if given-best (nth 2 given-best) 0)
                               (nth 1 score)))))
            (push (concat
                   (if given-best
                       (format ";;     %s %s: %s, best at %s" label given
                               (fontutil/format-score (cdr given-best))
                               (fontutil/format-size (car given-best) size))
                     (format ";;     %s %s: no glyph fits one cell"
                             label given))
                   (if worse " -- WORSE than the Latin font it replaces" ""))
                  lines)))
        ;; Suggest others unless the Latin or given font covers it all.
        (unless (or latin-ok
                    (and given-best (= (nth 2 given-best) (nth 3 given-best))))
          (push (if (eq role :punct)
                    (or punct-try
                        (setq punct-try
                              (fontutil/try-line
                               (fontutil/role-chars :punct) cell size
                               (delq nil (list latin punct))
                               "scored on all :punct groups")))
                  (fontutil/try-line chars cell size
                                     (delq nil (list latin given))))
                lines))))
    ;; A symbol font with only Private Use Area icons (e.g. Nerd Font
    ;; "Symbols" fonts) cannot help: the PUA is not part of the `symbol'
    ;; script that `:symbol' is mapped to.
    (when symbol
      (let ((sym-chars (fontutil/role-chars :symbol)))
        (when (and (zerop (car (fontutil/coverage symbol size cell sym-chars)))
                   (fontutil/glyph-width symbol size #xE0A0))
          (push (format ";;   NOTE: %s only has Private Use Area icons; as \
`:symbol' it has no effect" symbol)
                lines))))
    (nreverse lines)))

;;; ------------------------------------------------------------------
;;; Describing a font: facts and coverage of every group
;;; ------------------------------------------------------------------

(defun fontutil/font-facts (family size)
  "Return a plist of facts about FAMILY at pixel SIZE, or nil if missing.
  :mono     non-nil if the font says it is monospaced
  :em       advance of a space in em (Source Code Pro is 0.60); a
            monospaced Latin font's cell is about SIZE x :em pixels
  :height   ascent + descent in pixels at SIZE (the line height it
            needs; a taller helper font makes lines with its glyphs
            taller than the rest)
  :weights  available weights, e.g. (light regular bold)
  :italic   non-nil if there is an italic or oblique style
  :widths   available widths other than normal, e.g. (expanded)
  :icons    number of Private Use Area glyphs (Nerd Font icons)
Only the font object is queried; no frame is changed."
  (let* ((entity (find-font (font-spec :family family :size size)))
         (font (and entity (open-font entity size)))
         ;; [NAME FILENAME PIXEL-SIZE MAX-WIDTH ASCENT DESCENT SPACE-WIDTH
         ;;  AVERAGE-WIDTH CAPABILITY]; FILENAME is nil on macOS.
         (info (and font (query-font font)))
         (styles (list-fonts (font-spec :family family)))
         ;; Measure the em ratio at 100px so rounding does not hide it.
         (space100 (fontutil/glyph-width family 100 ?\s)))
    (when info
      (list :mono (and (find-font (font-spec :family family :spacing 100)) t)
            :em (and space100 (/ space100 100.0))
            :height (+ (aref info 4) (aref info 5))
            :weights (delete-dups (mapcar (lambda (e) (font-get e :weight))
                                          styles))
            :italic (seq-some (lambda (e)
                                (memq (font-get e :slant) '(italic oblique)))
                              styles)
            :widths (delq 'normal
                          (delete-dups (mapcar (lambda (e) (font-get e :width))
                                               styles)))
            :icons (seq-count (lambda (ch) (font-has-char-p entity ch))
                              (number-sequence #xE000 #xF8FF))))))

(defun fontutil/font-facts-line (family size &optional ref-height)
  "Return a one-line summary of FAMILY at pixel SIZE for a report.
REF-HEIGHT, if given, is the line height in pixels of the font it will
sit next to (the Latin font); the ratio is shown when it differs."
  (let ((f (fontutil/font-facts family size)))
    (if (null f)
        (format "%s: not installed" family)
      (format "%s @%dpx: %s, %.2f em, line %dpx%s, %s%s%s%s"
              family size
              (if (plist-get f :mono) "monospaced" "PROPORTIONAL")
              (or (plist-get f :em) 0)
              (plist-get f :height)
              (if (and ref-height (/= ref-height (plist-get f :height)))
                  (format " (%.2fx)" (/ (float (plist-get f :height))
                                        ref-height))
                "")
              (mapconcat #'symbol-name (plist-get f :weights) "/")
              (if (plist-get f :italic) " + italic" "")
              (if (plist-get f :widths)
                  (format ", widths %s"
                          (mapconcat #'symbol-name (plist-get f :widths) "/"))
                "")
              (if (> (plist-get f :icons) 0)
                  (format ", %d icons (PUA)" (plist-get f :icons))
                "")))))

;;;###autoload
(defun fontutil/describe-coverage (family &optional cell)
  "Show what FAMILY covers and how well it fits a CELL-pixel cell.
CELL defaults to the current default font's cell width; with a prefix
argument it is asked for.  Use this to judge a font before putting it
in `fontutil/families' (as `:latin', `:hangul', `:punct', `:symbol' or
`:math').  The *fontutil-coverage* buffer shows:

  Facts      see `fontutil/font-facts'; the line height is compared
             with the current default font.
  Groups     each group of `fontutil/describe-groups' and
             `fontutil/coverage-groups', scored at the size where the
             most of its characters have the width `char-width' wants
             (1 cell, 2 for Hangul/Hanja): HAVE/TOTAL glyphs, and how
             many of those have the wrong width.  `none fits' means no
             size aligns any glyph; the HAVE count is then at the size
             whose space is one cell.
  By role    the same, but over all groups of a role at ONE size, since
             a preset uses one size per role: this is what you get.

In a CJK language environment, Emacs counts ambiguous-width characters
\(some Latin-1, arrows, math) as 2 cells, so a font drawing them one
cell wide shows them as wrong width; that is Emacs's width table, not
the font."
  (interactive
   (list (completing-read "Font family: " (delete-dups (font-family-list))
                          nil t)
         (and current-prefix-arg
              (read-number "Cell width in pixels: " (default-font-width)))))
  (unless (display-graphic-p)
    (user-error "Run this in a graphic frame"))
  (let* ((cell (or cell (default-font-width)))
         (groups (append fontutil/describe-groups fontutil/coverage-groups))
         ;; Size used for the facts and for HAVE when nothing aligns: the
         ;; one whose space is one cell, else a 0.6 em guess.
         (ref-size (or (seq-find (lambda (s)
                                   (eql (fontutil/cell-width family s) cell))
                                 (fontutil/cell-sizes cell))
                       (round (/ cell 0.6))))
         (default-font (face-attribute 'default :font))
         (default-info (and (fontp default-font 'font-object)
                            (query-font default-font)))
         (default-height (and default-info
                              (+ (aref default-info 4) (aref default-info 5)))))
    (let ((row (lambda (name role chars)
                (let ((best (fontutil/best-cell-size family cell chars)))
                  (format "  %-13s %-8s %-14s %s\n" name role
                          (if best
                              (concat "@" (fontutil/format-size (car best)
                                                                ref-size))
                            "")
                          (if best
                              (fontutil/format-score (cdr best))
                            (let ((have (car (fontutil/coverage
                                              family ref-size cell chars))))
                              (if (zerop have)
                                  (format "0/%d" (length chars))
                                (format "%d/%d, none fits" have
                                        (length chars))))))))))
      (with-current-buffer (get-buffer-create "*fontutil-coverage*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "%s, measured against a %dpx cell (default font: %s)\n\n"
                          family cell
                          (if default-info
                              (format "%s %dpx" (font-get default-font :family)
                                      (aref default-info 2))
                            "?")))
          (insert "Facts\n  "
                  (fontutil/font-facts-line family ref-size default-height)
                  "\n\n")
          (insert "Groups (best size each; HAVE/TOTAL glyphs, and how many have the wrong width)\n")
          (dolist (g groups)
            (pcase-let ((`(,name ,role ,ranges) g))
              (insert (funcall row name role
                               (fontutil/role-group-chars role ranges)))))
          (insert "\nBy role (one size for all groups of the role)\n")
          (dolist (role '(:latin :hangul :hanja :punct :symbol :math))
            (insert (funcall row (substring (symbol-name role) 1) role
                             (fontutil/role-chars role groups)))))
        (goto-char (point-min))
        (special-mode)
        (pop-to-buffer (current-buffer))))))

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
                     ;; 字 is only checked when Hanja is managed here.
                     (append "a한" (and fontutil/enable-hanja "字")
                             fontutil/check-chars nil)))))
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
(defun fontutil/generate-family (id latin hangul punct symbol math sizes)
  "Measure sizes for a new font set and show a family entry to paste.
ID is the family id (a string, e.g. \"mymono\").  LATIN and HANGUL are
installed family names.  PUNCT, SYMBOL and MATH are family names or
nil/\"\":
  PUNCT  font for `fontutil/latin-ranges' (curly quotes, ellipsis, box
         drawing...).  It REPLACES the Latin font for all of them.  nil
         means the Latin font draws them, so no `:punct' is emitted.
  SYMBOL font for arrows, shapes and other `symbol' script characters
         the Latin font lacks.  nil means no symbol mapping (nil sizes
         and no `:symbol').
  MATH   font for `fontutil/math-ranges' (math operators and letters)
         the Latin font lacks, e.g. Noto Sans Math.  nil means math
         operators fall back to SYMBOL and math letters to Emacs's own
         fallback, so no `:math' is emitted.
SIZES is a list of Latin PIXEL sizes (the unit of `:size' in
`fontutil/families', not points).

Not sure whether PUNCT, SYMBOL or MATH is needed?  Leave them empty
first.  The coverage report at the top of the result
\(`fontutil/coverage-report') says, for each group of
`fontutil/coverage-groups', whether the Latin font draws it one cell
wide, and if not, which role (`:punct', `:symbol' or `:math') would
cover it and which installed fonts could, at what size.  Then run this
again with the fonts you picked to see what they cover.

For each size the Latin cell width is measured and these are fitted:
  Hangul  size at which 한 is exactly two cells wide.
  symbol  size at which the most `:symbol' group glyphs are one cell
          wide.
A Hangul size where nothing fits gets `auto' (`fontutil/apply' then
falls back to 2x the cell).  A symbol size where nothing fits gets nil,
so that preset has no symbol mapping.  The PUNCT and MATH sizes are not
put in the table because `fontutil/apply' measures them each time (PUNCT
via ─, MATH by `fontutil/fit-math-size'); each row's comment shows the
size that will be used, or that none fits.

Fonts are measured directly (`fontutil/glyph-widths'), so the frame's
fonts are not changed.  The result goes to the *fontutil-family*
buffer."
  (interactive
   (let ((fams (delete-dups (font-family-list))))
     (list (read-string "Family id (e.g. mymono): ")
           (completing-read "Latin family: " fams nil t)
           (completing-read "Hangul family: " fams nil t)
           (completing-read "Punctuation family (empty = use Latin): "
                            fams nil t)
           (completing-read "Symbol family (empty = none): " fams nil t)
           (completing-read "Math family (empty = use symbol): "
                            fams nil t)
           (mapcar #'string-to-number
                   (split-string
                    (read-string "Latin pixel sizes (space separated): "
                                 "14 16 18 20"))))))
  (unless (display-graphic-p)
    (user-error "Run this in a graphic frame"))
  (when (equal punct "") (setq punct nil))
  (when (equal symbol "") (setq symbol nil))
  (when (equal math "") (setq math nil))
  (dolist (f (delq nil (list latin hangul punct symbol math)))
    (unless (find-font (font-spec :family f))
      (user-error "Font family %S not found" f)))
  ;; `string-to-number' turns a typo such as "1x" into 0, so check here.
  (unless (and sizes (seq-every-p (lambda (s) (and (integerp s) (> s 0)))
                                  sizes))
    (user-error "Sizes must be positive integer pixel sizes, got %S" sizes))
  (unless (fontutil/glyph-width hangul (car sizes) ?한)
    (user-error "Hangul family %S has no 한; pick another" hangul))
  (let* ((sym-chars (fontutil/role-chars :symbol))
         ;; One row per size: (SIZE CELL HSIZE SSIZE PSIZE MSIZE).  Every
         ;; value is measured from the fonts themselves for that cell.
         (rows
          (mapcar
           (lambda (size)
             (let ((cell (or (fontutil/cell-width latin size)
                             (user-error "%S has no space glyph" latin))))
               (list size cell
                     (fontutil/fit-hangul-size hangul cell)
                     (and symbol
                          (car (fontutil/best-cell-size symbol cell
                                                        sym-chars)))
                     (and punct (fontutil/fit-punct-size punct nil cell))
                     (and math (fontutil/fit-math-size math cell)))))
           sizes))
         ;; The cell width comes from the space; if letters differ from it
         ;; the font is proportional and nothing will line up.
         (cell0 (nth 1 (car rows)))
         (proportional
          (seq-some (lambda (w) (not (eql w cell0)))
                    (fontutil/glyph-widths latin (car sizes)
                                           (string-to-list "aiMW0")))))
    (with-current-buffer (get-buffer-create "*fontutil-family*")
      (erase-buffer)
      (when proportional
        (insert (format ";; WARNING: %s is not monospaced (a i M W 0 are \
not all %dpx); pick another Latin font\n" latin cell0)))
      ;; Facts on each chosen font, at the sizes of the first row.  The
      ;; line height ratio is against the Latin font: a helper font that
      ;; is taller makes the lines where its glyphs appear taller.
      (pcase-let* ((`(,size ,_cell ,hsize ,ssize ,psize ,msize) (car rows))
                   (ref-height (plist-get (fontutil/font-facts latin size)
                                          :height)))
        (insert ";; Fonts (sizes of the first row; see `fontutil/font-facts')\n")
        (dolist (f (list (list "Latin" latin size)
                         (list "Hangul" hangul (or hsize (* 2 (nth 1 (car rows)))))
                         (list ":punct" punct (or psize size))
                         (list ":symbol" symbol (or ssize size))
                         (list ":math" math (or msize size))))
          (when (nth 1 f)
            (insert (format ";;   %-8s %s\n" (car f)
                            (fontutil/font-facts-line (nth 1 f) (nth 2 f)
                                                      ref-height))))))
      (dolist (line (fontutil/coverage-report latin (car sizes) punct symbol
                                              math))
        (insert line "\n"))
      (insert "\n")
      (insert (format "(%s :latin (%S) :hangul (%S)\n" id latin hangul))
      (when punct (insert (format " :punct (%S)\n" punct)))
      (when symbol (insert (format " :symbol (%S)\n" symbol)))
      (when math (insert (format " :math (%S)\n" math)))
      (insert " :sizes (\n")
      (dolist (r rows)
        (pcase-let ((`(,size ,cell ,hsize ,ssize ,psize ,msize) r))
          (insert (format "   (%d %s %s)   ; cell %dpx%s%s%s%s\n"
                          size (or hsize 'auto) ssize cell
                          (if hsize "" ", hangul: no fit, using auto")
                          (if (or (null symbol) ssize) ""
                            ", symbol: no glyph fits, no mapping")
                          (if (or (null punct) psize) ""
                            ", punct: ─ fits no size, pick another font")
                          (cond ((null math) "")
                                (msize (format ", math %dpx" msize))
                                (t ", math: no glyph fits"))))))
      (insert "   ))\n")
      (emacs-lisp-mode)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun fontutil/suggest-hangul-size ()
  "Measure the Hangul size that fits the current preset."
  (interactive)
  (let* ((p (or (fontutil/preset fontutil/current)
                (user-error "Select a font with `fontutil/set-font' first")))
         (family (plist-get (plist-get p :hangul) :family))
         ;; Measuring does not change the fontset, so nothing to restore.
         (size (fontutil/fit-hangul-size family)))
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
    ;; (clear-font-cache)  ; can free fonts still used by faces → crash
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
    ;; cells).  Nothing is done unless `fontutil/enable-hanja' is set
    ;; (presets built while it was nil have no :hanja anyway; the check
    ;; here also covers presets cached before it was turned off).
    (let* ((hanja (plist-get preset :hanja))
           (family (and fontutil/enable-hanja (plist-get hanja :family)))
           (has-hanja (and family
                           (font-has-char-p
                            (find-font (font-spec :family family)) ?字)))
           (size (and has-hanja (fontutil/fit-hanja-size family))))
      (cond
       (size (fontutil/set-hanja-font family size))
       ((null family))                  ; Hanja disabled: leave it alone
       ((not has-hanja)
        ;; The font has no Hanja glyphs, so there is no size to search
        ;; for and mapping Hanja to it would be pointless.  Set `:hanja'
        ;; for the family, or install a CJK mono font (see
        ;; `fontutil/hanja-families').
        (message "fontutil: %s has no Hanja (字); %s"
                 family "Hanja left to Emacs's fallback"))
       (t
        (message "fontutil: no aligned Hanja size for %s with %s"
                 (plist-get preset :name) family)
        ;; Map Hanja to a sane guess (2x cell) so the previous preset's
        ;; size does not linger.  Do NOT pass nil to `set-fontset-font':
        ;; that means "no font" and would blank these characters.
        (fontutil/set-hanja-font family (* 2 (default-font-width))))))
    ;; Math: the `:math' font draws `fontutil/math-ranges', sized so the
    ;; most math glyphs are one cell wide (a math font cannot fit them
    ;; all).  This runs after the `symbol' mapping above, so for math
    ;; operators it wins over `:symbol'; the Latin font still draws the
    ;; operators it has (`use-default-font-for-symbols').
    (let ((mfamily (plist-get (plist-get preset :math) :family)))
      (cond
       (mfamily
        (fontutil/map-ranges
         fontutil/math-ranges mfamily
         (or (fontutil/fit-math-size mfamily)
             (progn
               (message "fontutil: no one-cell math size for %s with %s"
                        (plist-get preset :name) mfamily)
               (plist-get latin :size))))
        (setq fontutil--math-mapped t))
       (fontutil--math-mapped
        ;; No `:math' here, but an earlier preset mapped these ranges.
        ;; Re-map them as if no math font had ever been set.  Math
        ;; operators are `symbol' script, so they go to the `:symbol'
        ;; font if there is one.  Everything else gets an empty
        ;; `font-spec', which makes Emacs search all fonts for the
        ;; glyph, as a fresh session does.  Tested with 𝐀: a named font
        ;; lacking the glyph, nil, `(nil . "iso10646-1")' and
        ;; `(font-spec :registry "iso10646-1")' all left it with NO
        ;; font; only the empty spec brought the search back.
        (let ((ssize (plist-get symbol :size)))
          (dolist (range fontutil/math-ranges)
            (set-fontset-font
             t range
             (if (and symbol
                      (eq (aref char-script-table (car range)) 'symbol))
                 (font-spec :family (plist-get symbol :family)
                            :size (if (integerp ssize) ssize
                                    (plist-get latin :size)))
               (font-spec))))))))
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

