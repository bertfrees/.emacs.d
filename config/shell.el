;; -----------------------------------------------------------------------------
;; SHELL
;; -----------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(eval-after-load 'sh-mode '(progn
  (add-hook 'sh-mode-hook
    (lambda ()
      (when (display-graphic-p)
        (whitespace-mode t))))))
