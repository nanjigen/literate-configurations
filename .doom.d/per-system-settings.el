(require 'map) ;; Needed for map-merge

(setq nanjigen/system-settings
  (map-merge
    'list
    '((desktop/dpi . 180)
      (desktop/background . "~/Pictures/Backgrounds/deskwall.jpg")
      (polybar/height . 35)
;; (setq doom-font (font-spec :family "Ubuntu Mono" :size 16))
      (polybar/font-0-size . 18)
      (polybar/font-1-size . 14)
      (polybar/font-2-size . 20)
      (polybar/font-3-size . 13)
      (dunst/font-size . 20)
      (dunst/max-icon-size . 88))
    
    (when (equal system-name "espeon"))
    
    (when (equal system-name "umbreon")
      '((desktop/dpi . 130)
        (polybar/height . 25)
        (polybar/font-0-size . 12)
        (polybar/font-1-size . 8)
        (polybar/font-2-size . 14)
        (polybar/font-3-size . 9)
        (dunst/font-size . 14)
        (dunst/max-icon-size . 64)
        (vimb/default-zoom . 150)))
    
    (when (equal system-name "flareon")
      '((desktop/dpi . 240)
        (polybar/height . 40)
        (vimb/default-zoom . 200)))
    ))
