;; -----------------------------------------------------------------------------
;; CLOJURE
;; -----------------------------------------------------------------------------

(require-package 'clojure-mode 'clojure-mode)

(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

(eval-after-load 'clojure-mode '(progn
  
  (add-hook 'clojure-mode-hook
    (lambda () (local-set-key (kbd "RET")
                '(lambda() (interactive)
                           (newline)
                           (indent-according-to-mode)))
               (setq indent-tabs-mode nil)
               (paren-activate)
               (rainbow-delimiters-mode t)
               (auto-complete-mode t)))))

;; -----------------------------------------------------------------------------
;; NREPL
;; -----------------------------------------------------------------------------

(require-package 'nrepl 'nrepl-jack-in t)

(eval-after-load 'nrepl '(progn
  
  (require-package 'ac-nrepl)
  
  (setq nrepl-popup-stacktraces nil)
  (setq nrepl-popup-stacktraces-in-repl nil)
  (add-to-list 'same-window-buffer-names "*nrepl*")
  
  (add-hook 'nrepl-mode-hook
    (lambda () (paren-activate)
               (rainbow-delimiters-mode t)
               (nrepl-turn-on-eldoc-mode)
               (auto-complete-mode t)
               (require 'ac-nrepl)
               (setq ac-sources '(ac-source-nrepl-ns
                                  ac-source-nrepl-vars
                                  ac-source-nrepl-ns-classes
                                  ac-source-nrepl-all-classes
                                  ac-source-nrepl-java-methods
                                  ac-source-nrepl-static-methods))))
  
  (add-hook 'nrepl-interaction-mode-hook
    (lambda () (paren-activate)
               (rainbow-delimiters-mode t)
               (nrepl-turn-on-eldoc-mode)))))

;; -----------------------------------------------------------------------------

(defun clojure-repl ()
  (interactive)
  (when (not (comint-check-proc "*clojure-repl*"))
    (set-buffer (make-comint "clojure-repl" "clj"))
    (inferior-lisp-mode))
  (setq inferior-lisp-buffer "*clojure-repl*")
  (pop-to-buffer-same-window "*clojure-repl*"))

;; -----------------------------------------------------------------------------
;; CLOJURESCRIPT
;; -----------------------------------------------------------------------------

(require-package 'clojurescript-mode 'clojurescript-mode t)

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))

(eval-after-load 'clojurescript-mode '(progn
  
  (require-package 'find-file-in-project)
  (require 'find-file-in-project)
  
  (defun clojurescript-start-cljs-repl ())
  
  ;; don't enable nrepl-mode in clojurescript-mode
  (defun clojure-enable-nrepl ()
    (when (and (not (eq major-mode 'clojurescript-mode))
               nrepl-connection-list)
      (nrepl-interaction-mode 1)
      (setq next-error-function 'nrepl-jump-to-compilation-error)))
  
  (add-hook 'clojurescript-mode-hook
    (lambda () (set (make-local-variable 'inferior-lisp-program)
                    (expand-file-name "./browser-repl" (ffip-project-root)))))))
