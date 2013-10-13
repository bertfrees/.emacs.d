;; -----------------------------------------------------------------------------
;; IRC
;; -----------------------------------------------------------------------------

(eval-after-load "erc" '(progn
  
  ;; history
  (setq erc-log-channels-directory "~/.erc/logs/")
  (setq erc-save-buffer-on-part t)
  (setq erc-hide-timestamps t)))
