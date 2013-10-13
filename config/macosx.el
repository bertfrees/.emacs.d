;; -----------------------------------------------------------------------------
;; MAC OS KEY BINDINGS
;; -----------------------------------------------------------------------------

(when (eq system-type 'darwin)

  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)

  (defun mac-osx-editing-insert-tilde ()
    "Insert ~ at point"
    (interactive)
    (insert-char ?~ 1))
  (defun mac-osx-editing-insert-curly-bracket-left ()
    "Insert { at point"
    (interactive)
    (insert-char ?{ 1))
  (defun mac-osx-editing-insert-curly-bracket-right ()
    "Insert } at point"
    (interactive)
    (insert-char ?} 1))
  (defun mac-osx-editing-insert-square-bracket-left ()
    "Insert [ at point"
    (interactive)
    (insert-char ?[ 1))
  (defun mac-osx-editing-insert-square-bracket-right ()
    "Insert ] at point"
    (interactive)
    (insert-char ?] 1))
  (defun mac-osx-editing-insert-pipe ()
    "Insert | at point"
    (interactive)
    (insert-char ?| 1))
  (defun mac-osx-editing-insert-back-slash ()
    "Insert \ at point"
    (interactive)
    (insert-char ?\\ 1))
  (defun mac-osx-editing-insert-euro-sign ()
    "Insert € at point"
    (interactive)
    (insert-char ?€ 1))

  (global-set-key (kbd "M-n") 'mac-osx-editing-insert-tilde)
  (global-set-key (kbd "M-5") 'mac-osx-editing-insert-square-bracket-left)
  (global-set-key (kbd "M-°") 'mac-osx-editing-insert-square-bracket-right)
  (global-set-key (kbd "M-(") 'mac-osx-editing-insert-curly-bracket-left)
  (global-set-key (kbd "M-)") 'mac-osx-editing-insert-curly-bracket-right)
  (global-set-key (kbd "M-L") 'mac-osx-editing-insert-pipe)
  (global-set-key (kbd "M-/") 'mac-osx-editing-insert-back-slash)
  (global-set-key (kbd "M-$") 'mac-osx-editing-insert-euro-sign)
  
  ;; fix electric-pair-mode
  (add-hook 'electric-pair-mode-hook (lambda ()
    (global-set-key (kbd "M-(")
     '(lambda ()
        (interactive)
        (insert-char ?{ 1)
        (save-excursion (insert-char ?} 1))))))
  
  ;; growl
  (require-package 'growl 'growl)
  
  ;; share clipboard in terminal mode
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))
