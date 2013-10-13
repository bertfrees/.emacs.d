;; -----------------------------------------------------------------------------
;; JAVA
;; -----------------------------------------------------------------------------

(add-hook 'java-mode-hook
  (lambda () (eclim-mode)
             (setq fill-column 100)))

;; -----------------------------------------------------------------------------
;; ECLIM
;; -----------------------------------------------------------------------------

(require-package 'emacs-eclim 'eclim-mode t)

(eval-after-load 'eclimd '(progn
  
  (require-package 'find-file-in-project)
  (require 'find-file-in-project)
  
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)
  
  (setq eclim-auto-save nil)
  (setq eclim-use-yasnippet t)
  (setq eclimd-default-workspace "~/dev")
  (setq eclim-eclipse-dirs '("/Applications/Eclipse" "/opt/eclipse"))
  (define-key eclim-problems-mode-map [double-mouse-1] 'eclim-problems-open-current)
  
  (add-hook 'eclim-mode-hook
    (lambda () (eclimd-auto-start)))
  
  ;; auto-complete
  (require 'ac-emacs-eclim-source)
  
  ;; only do auto-save when ac is triggered with TAB
  (defadvice ac-trigger-key-command (before save-buffer-before-ac-trigger activate)
    (if (and eclim-mode (eclimd--running-p))
      (save-buffer)))))

;; -----------------------------------------------------------------------------

(defun eclimd-running ()
  (or (eclimd--running-p)
      (condition-case nil
          (progn (eclim/execute-command "ping") t)
        (error nil))))

(defvar eclimd-enable-auto-start t)

(defun eclimd-auto-start ()
  (unless (or (eclimd-running)
              (not eclimd-enable-auto-start))
    (if (yes-or-no-p "Start eclimd? ")
        (start-eclimd eclimd-default-workspace)
      (setq eclimd-enable-auto-start nil))))
