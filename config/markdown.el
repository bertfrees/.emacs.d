;; -----------------------------------------------------------------------------
;; MARKDOWN
;; -----------------------------------------------------------------------------

(require-package 'markdown-mode 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(add-hook 'markdown-mode-hook
  (lambda () (setq indent-tabs-mode nil)))
