;;; muse-palenight-theme.el --- inspired by Material-PaleNight
;;; Commentary:
;; Muse themes implementation of Material Palenight (thanks to Brettm12345)
(require 'muse-themes)

;;; Code:
(defgroup muse-palenight-theme nil
  "Options for muse-themes"
  :group 'muse-themes)

(defcustom muse-palenight-padded-modeline muse-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'muse-palenight-theme
  :type '(or integer boolean))

;;
(def-muse-theme muse-palenight
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         '("#292D3E" nil       nil))
   (bg-alt     '("#1C1E29" nil       nil))
   (base0      '("#1C1F2B" "black"   "black"))
   (base1      '("#1e212e" "#262626" "brightblack"))
   (base2      '("#242837" "#303030" "brightblack"))
   (base3      '("#3C435E" "#3a3a3a" "brightblack"))
   (base4      '("#4E5579" "#444444" "brightblack"))
   (base5      '("#676E95" "#585858" "brightblack"))
   (base6      '("#697098" "#626262" "brightblack"))
   (base7      '("#717CB4" "#767676" "brightblack"))
   (base8      '("#A6ACCD" "#a8a8a8" "white"))
   (fg         '("#EEFFFF" "#e4e4e4" "brightwhite"))
   (fg-alt     '("#BFC7D5" "#bcbcbc" "white"))

   (grey base5)

   (red         '("#ff5370" "#ff0000" "red"))
   (orange      '("#f78c6c" "#ff5f00" "brightred"))
   (green       '("#c3e88d" "#afff00" "green"))
   (teal        '("#44b9b1" "#00d7af" "brightgreen"))
   (yellow      '("#ffcb6b" "#ffd700" "brightyellow"))
   (blue        '("#82aaff" "#5fafff" "brightblue"))
   (dark-blue   '("#b2ccd6" "#d7ffff" "blue"))
   (magenta     '("#c792ea" "#d787d7" "brightmagenta"))
   (violet      '("#bb80b3" "#d787af" "magenta"))
   (cyan        '("#78ccf0" "#5fd7ff" "brightcyan"))
   (dark-cyan   '("#80cbc4" "#00d7af" "cyan"))

   ;; face categories -- required for all themes
   (highlight      magenta)
   (vertical-bar   "#282b3c")
   (selection      base4)
   (builtin        blue)
   (comments       base5)
   (doc-comments   (muse-lighten base5 0.25))
   (constants      blue)
   (functions      blue)
   (keywords       magenta)
   (methods        cyan)
   (operators      cyan)
   (type           yellow)
   (strings        green)
   (variables      yellow)
   (numbers        orange)
   (region         "#3c435e")
   (error          "#ff5572")
   (warning        "#ffca28")
   (success        green)
   (vc-modified    blue)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     base2)
   (modeline-bg-alt (muse-darken bg 0.01))
   (modeline-fg     base8)
   (modeline-fg-alt comments)

   (-modeline-pad
    (when muse-palenight-padded-modeline
      (if (integerp muse-palenight-padded-modeline) muse-palenight-padded-modeline 4))))

  ;; --- base faces ------------------------
  (((lazy-highlight &override) :background base4 :foreground fg :distant-foreground fg :bold bold)
   (muse-modeline-buffer-path       :foreground green :weight 'bold)
   (muse-modeline-buffer-major-mode :inherit 'muse-modeline-buffer-path)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))

   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   (solaire-mode-line-face
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (solaire-mode-line-inactive-face
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   (fringe :background base2)

   ((line-number-current-line &override) :foreground fg)

   (font-lock-keyword-face
    :slant 'italic
    :weight 'medium
    :foreground keywords)

   (font-lock-comment-face
    :slant 'italic
    :weight 'medium
    :foreground comments)

   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :slant 'italic
    :weight 'medium
    :foreground doc-comments)

   ;; --- major-mode faces ------------------------
   ;; man-mode
   (Man-overstrike :inherit 'bold :foreground magenta)
   (Man-underline :inherit 'underline :foreground blue)

   ;; org-mode
   (org-block                    :background base2)
   (org-block-background         :background base2)
   (org-block-begin-line         :foreground comments :background base2)

   ;; --- plugin faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; dired-k
   (dired-k-commited :foreground base4)
   (dired-k-modified :foreground vc-modified)
   (dired-k-ignored :foreground cyan)
   (dired-k-added    :foreground vc-added)


   ;; js2-mode
   (js2-jsdoc-tag              :foreground magenta)
   (js2-object-property        :foreground yellow)
   (js2-object-property-access :foreground cyan)
   (js2-function-param         :foreground violet)
   (js2-jsdoc-type             :foreground base8)
   (js2-jsdoc-value            :foreground cyan)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground blue)
   (rainbow-delimiters-depth-8-face :foreground teal)
   (rainbow-delimiters-depth-9-face :foreground dark-cyan)

   ;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground yellow :slant 'italic :weight 'medium)))

;;; muse-palenight-theme.el ends here
