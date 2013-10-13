;; -----------------------------------------------------------------------------
;; APPEARANCE
;; -----------------------------------------------------------------------------

;; tomorrow theme
(require-package 'color-theme-sanityinc-tomorrow)
;; colors
(require 'color-theme-sanityinc-tomorrow)
(setq color-theme-sanityinc-tomorrow-colors
  '((day . ((background   . "#ffffff")
            (current-line . "#efefef")
            (selection    . "#d6d6d6")
            (foreground   . "#151515")
            (comment      . "#8e908c")
            (red          . "#c82829")
            (orange       . "#ff6c1b")
            (yellow       . "#dca000")
            (green        . "#568c00")
            (aqua         . "#2994a1")
            (blue         . "#1d58d0")
            (purple       . "#9940a0")))))
(color-theme-sanityinc-tomorrow-day)

;; diff colors
(eval-after-load 'diff-mode '(progn
  (set-face-foreground 'diff-removed "#151515")
  (set-face-foreground 'diff-added "#151515")
  (set-face-background 'diff-removed "#ffccbb")
  (set-face-background 'diff-added "#ccffcc")
  (condition-case nil
      ;; Emacs 24.3+ only
      (progn
        (set-face-background 'diff-refine-removed "#ff8040")
        (set-face-background 'diff-refine-added "#60e040"))
    (error (set-face-background 'diff-refine-change "#d0a0e0")))))

;; other colors
(custom-set-faces
 '(font-lock-doc-string-face ((t (:slant italic :foreground "#4271ae"))))
 '(font-lock-warning-face ((t (:foreground "firebrick1" :underline "firebrick1")))))

;; alternative font on Mac
(if (eq system-type 'darwin)
  (custom-set-faces '(default ((t (:height 140 :family "Inconsolata"))))))

;; no toolbar
(if (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; no menubar in terminal
(unless window-system (menu-bar-mode -1))

;; smaller fringe
(if (fboundp 'fringe-mode)
  (fringe-mode 4))

;; current line highlighting
(global-hl-line-mode +1)

;; line numbering
(require-package 'linum-ex 'linum-mode t)
(eval-after-load 'linum-ex '(progn
  (custom-set-variables
    '(linum-format (quote dynamic)))
  (custom-set-faces
   '(linum ((t (:inherit (shadow default) :background "#F0F0F0" :foreground "#707070")))))))

;; Highlight fill-column
(require-package 'fill-column-indicator 'fci-mode t)
(setq-default fci-rule-column nil)
(eval-after-load 'fill-column-indicator '(progn
  (custom-set-variables '(fci-rule-color "#cccccc"))))

