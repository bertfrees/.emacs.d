;; -----------------------------------------------------------------------------
;; AUTO-COMPLETE
;; -----------------------------------------------------------------------------

(require-package 'auto-complete 'auto-complete-mode)

(setq-default ac-sources '())

(eval-after-load 'auto-complete '(progn
  
  (require 'auto-complete-config)
  
  (setq ac-auto-start nil)
  (ac-set-trigger-key "TAB")
  (setq ac-dwim t)
  (setq ac-ignore-case t)
  (setq ac-menu-height 10)
  (setq ac-quick-help-prefer-pos-tip t)
  (setq ac-use-quick-help t)
  (setq ac-quick-help-height 20)
  (setq ac-quick-help-delay 1)
  (setq ac-use-comphist nil)
  
  ;; -----------------------------------------------------------------------------
  
  ;; hack for fixing auto-complete + whitespace-mode conflict
  (set-default 'whitespace-mode nil)
  (set-default 'saved-whitespace-mode nil)
  
  (defadvice ac-menu-create (before turn-off-whitespace-before-auto-complete activate)
    (make-local-variable 'saved-whitespace-mode)
    (if (or whitespace-mode saved-whitespace-mode)
      (progn (setq saved-whitespace-mode t)
             (whitespace-mode -1))
      (setq saved-whitespace-mode nil)))
  
  (defadvice ac-menu-delete (after restore-whitespace-after-auto-complete activate)
    (make-local-variable 'saved-whitespace-mode)
    (if saved-whitespace-mode
      (progn (whitespace-mode 1)
             (setq saved-whitespace-mode nil))))))

;; -----------------------------------------------------------------------------
;; YASNIPPET
;; -----------------------------------------------------------------------------

(require-package 'yasnippet 'yas-minor-mode)

(eval-after-load 'yasnippet '(progn
  
  (setq yas/snippet-dirs (list (expand-file-name "snippets" emacs-dir)))
  
  (yas-reload-all)
  (setq yas/triggers-in-field t)
  
  (defun yas-skip-and-clear-or-delete-backward-char (&optional field)
    "Clears unmodified field if at field start, skips to next tab. Otherwise deletes a character normally by calling `delete-backward-char'."
    (interactive)
    (let ((field (or field
                     (and yas--active-field-overlay
                          (overlay-buffer yas--active-field-overlay)
                          (overlay-get yas--active-field-overlay 'yas--field)))))
      (cond ((and field
                  (not (yas--field-modified-p field))
                  (eq (point) (marker-position (yas--field-start field))))
             (yas--skip-and-clear field)
             (yas-next-field 1))
            (t
             (call-interactively 'delete-backward-char)))))
  
  (define-key yas-keymap (kbd "<backspace>") 'yas-skip-and-clear-or-delete-backward-char)
  
  (defun yas-next-field-or-maybe-expand-or-ac-complete ()
    (interactive)
    (let ((yas-fallback-behavior 'return-nil)
          (active-field (overlay-get yas--active-field-overlay 'yas--field)))
      (when active-field
        (unless (yas-expand-from-trigger-key active-field)
          (let (started)
            (when (ac-trigger-command-p last-command)
              (setq started (auto-complete-1 :triggered 'trigger-key)))
            (unless (and started (ac-candidates))
              (yas-next-field)))))))
  
  (define-key yas-keymap (kbd "<tab>") 'yas-next-field-or-maybe-expand-or-ac-complete)))
