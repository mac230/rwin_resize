;;;;;;;;;;
;; face attributes
;; 2019.07.08 - started changing these, mainly to background = nil for using color themes

;; a useful function for determining whether or not a color exists is 'color-defined-p'
;; (color-defined-p "white")

;; return a list of defined colors:
;; (defined-colors)

;; return integer values for a defined color, e.g., "blue" = (0 0 65280) -> 16 bit
;; divide by 2^8 to get in 8 bit scale
;; (color-values "light green")

(set-face-attribute 'ivy-minibuffer-match-face-1 nil                   :background nil)
(set-face-attribute 'ivy-minibuffer-match-face-2 nil                   :background nil)
(set-face-attribute 'ivy-minibuffer-match-face-3 nil                   :background nil)
(set-face-attribute 'ivy-minibuffer-match-face-4 nil                   :background nil)
(set-face-attribute 'org-level-4 nil                                   :background nil)

(set-face-attribute 'font-lock-keyword-face nil                   :background nil)
(set-face-attribute 'font-lock-comment-delimiter-face nil         :background nil)
(set-face-attribute 'dired-subtree-depth-1-face nil               :background nil)
(set-face-attribute 'dired-subtree-depth-2-face nil               :background nil)
(set-face-attribute 'dired-subtree-depth-3-face nil               :background nil)
(set-face-attribute 'dired-subtree-depth-4-face nil               :background nil)
(set-face-attribute 'diredp-write-priv nil                        :foreground "#501480" :background "#F1F1F1")
(set-face-attribute 'diredp-read-priv nil                         :foreground "#501480" :background "#F1F1F1")
(set-face-attribute 'diredp-no-priv nil                           :foreground "#501480" :background "#F1F1F1")
(set-face-attribute 'diredp-exec-priv nil                         :foreground "#501480" :background "#F1F1F1")
(set-face-attribute 'diredp-file-name nil                         :foreground "#223399" :background "#FFFFFF")
(set-face-attribute 'diredp-dir-heading nil                       :foreground "#551155" :background "#F1F1F1")
(set-face-attribute 'company-tooltip nil                          :foreground "#111111" :background nil) ;;the non-highlighted options, white w/ black text here
(set-face-attribute 'company-scrollbar-bg nil                     :background "#E8E8E8")                       ;;the background of the scrollbar, light gray here
(set-face-attribute 'company-scrollbar-fg nil                     :background "#551155")                       ;;the scrollbar itself, confusingly, need to set w/ background, dark purple here
(set-face-attribute 'company-tooltip-selection nil                :foreground "#551155" :background "#E8E8E8") ;;the highlighted selection, gray w/ purple here
(set-face-attribute 'company-tooltip-common nil                   :foreground "#551155" :background "#E8E8E8") ;;the text common to each option, gray w/ purple here
(setq company-quickhelp-color-background "#EFEFEF")
(setq company-quickhelp-color-foreground "#551155")
(set-face-attribute 'tooltip nil :family "Menlo"                  :foreground "#551155" :background "#EFEFEF")   ;;get the pos-tip font I want
(set-face-attribute 'org-special-keyword nil                      :foreground "#7575FF" :background "#E7E8E9")

;; rgb calculator - http://drpeterjones.com/colorcalc/
