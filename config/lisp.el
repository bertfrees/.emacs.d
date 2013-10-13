;; -----------------------------------------------------------------------------
;; LISP
;; -----------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))

(require-package 'rainbow-delimiters 'rainbow-delimiters-mode)
(require-package 'mic-paren 'paren-activate)

(require-package 'nrepl-eval-sexp-fu)

(require 'nrepl-eval-sexp-fu)

(setq nrepl-eval-sexp-fu-flash-duration 0.5)
(custom-set-faces '(nrepl-eval-sexp-fu-flash ((t (:slant italic :weight ultra-bold)))))

;; add inferior-lisp support
(define-nrepl-eval-sexp-fu-flash-command lisp-eval-last-sexp
  (nrepl-eval-sexp-fu-flash
   (save-excursion
     (backward-char)
     (bounds-of-thing-at-point 'sexp))))

(add-hook 'emacs-lisp-mode-hook
  (lambda () (local-set-key (kbd "RET")
              '(lambda() (interactive)
                         (newline)
                         (indent-according-to-mode)))
             (setq indent-tabs-mode nil)
             (paren-activate)
             (rainbow-delimiters-mode +1)
             (auto-complete-mode t)
             (setq ac-sources '(ac-source-features
                                ac-source-functions
                                ac-source-variables
                                ac-source-symbols
                                ac-source-filename
                                ac-source-abbrev
                                ac-source-dictionary
                                ac-source-words-in-same-mode-buffers))))

(add-hook 'lisp-interaction-mode-hook
  (lambda () (paren-activate)
             (rainbow-delimiters-mode t)))
