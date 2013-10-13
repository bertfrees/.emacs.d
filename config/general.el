;; -----------------------------------------------------------------------------
;; GENERAL
;; -----------------------------------------------------------------------------

;; start server
(require 'server)
(or (server-running-p) (server-start))

;; edit-server for Chrome's `Edit with Emacs' plugin
(require-package 'edit-server)
(when (and (require 'edit-server nil t) (daemonp))
  (edit-server-start)
  ;; fix for Gmail etc.
  (require-package 'edit-server-htmlize)
  (add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
  (add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer))

;; no initial scratch message
(setq initial-scratch-message nil)

;; no startup screen
(setq inhibit-startup-screen t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; navigation
(require-package 'emacs-nav 'nav t)

;; ido
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-default-file-method 'selected-window)
(add-to-list 'ido-ignore-files "\\.DS_Store") ;; Ignore .DS_Store & Icon files
(add-to-list 'ido-ignore-files "\\Icon")

;; beep is annoying
(setq ring-bell-function 'ignore)

;; undo tree
(require-package 'undo-tree)
(global-undo-tree-mode)

;; revert buffers automatically
(global-auto-revert-mode t)

;; smart pairing
(electric-pair-mode t)

;; miscellaneous
(setq line-move-visual nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(set-keyboard-coding-system nil)
