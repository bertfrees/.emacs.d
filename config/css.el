;; -----------------------------------------------------------------------------
;; CSS
;; -----------------------------------------------------------------------------

(require-package 'css-mode 'css-mode)
(require-package 'sass-mode 'sass-mode)
(require-package 'less-css-mode 'less-css-mode)

(add-to-list 'auto-mode-alist '("\\.css\\'"  . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))

(eval-after-load 'css-mode '(progn
  
  (setq-default css-indent-level 4)

  (add-hook 'css-mode-hook
    (lambda () (local-set-key (kbd "RET") 'newline-and-indent)
               (guess-style-guess-variable 'indent-tabs-mode)
               (guess-style-guess-variable 'tab-width)
               (guess-style-guess-variable 'css-indent-level 'guess-style-guess-indent)
               (smart-tabs-mode t)
               (when (display-graphic-p)
                 (whitespace-mode t))
               (setq ac-sources '(ac-source-css-property))))))
