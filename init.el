;; -----------------------------------------------------------------------------
;; Variables
;; -----------------------------------------------------------------------------

(defvar emacs-dir (file-name-directory load-file-name))
(defvar el-get-dir (expand-file-name "el-get/" emacs-dir))
(defvar recipes-dir (expand-file-name "recipes/" emacs-dir))
(defvar patches-dir (expand-file-name "patches/" emacs-dir))
(defvar config-dir (expand-file-name "config/" emacs-dir))

;; -----------------------------------------------------------------------------
;; Paths
;; -----------------------------------------------------------------------------

(defun add-to-exec-path (dir)
  (setenv "PATH" (concat dir ":" (getenv "PATH")))
  (push dir exec-path))

(add-to-exec-path "/usr/local/bin")

(when (eq system-type 'darwin)
  (add-to-exec-path "/usr/local/git/bin")
  (add-to-exec-path "/usr/local/share/python"))

;; -----------------------------------------------------------------------------
;; el-get
;; -----------------------------------------------------------------------------

(unless (file-exists-p (concat el-get-dir "el-get"))
  (with-current-buffer
      (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let* (el-get-master-branch)
      (end-of-buffer)
      (eval-print-last-sexp))))

(add-to-list 'load-path (expand-file-name "el-get/" el-get-dir))

(require 'el-get)

(setq el-get-recipe-path (list recipes-dir))

(setq el-get-sources
  '((:name melpa :type elpa)))

(el-get 'sync 'package)

(defun patch-package (package)
  (list "patch" "-p1"
        "-i" (expand-file-name (concat (el-get-as-string package) ".patch") patches-dir)
        "-d" (el-get-package-directory package)))

(defun require-package (package-or-recipe &optional function interactive)
  (let* ((package (if (symbolp package-or-recipe) package-or-recipe (plist-get package-or-recipe :name)))
         (pname (el-get-as-string package))
         (deferred-file (concat el-get-dir pname ".el")))
    (if (or (el-get-package-installed-p package)
            (not function))
      (progn
        (unless (symbolp package-or-recipe)
          (add-to-list 'el-get-sources package-or-recipe))
        (el-get 'sync package)
        (when (file-exists-p deferred-file)
          (delete-file deferred-file)))
      (progn
        (unless (file-exists-p deferred-file)
          (append-to-file (concat "(require-package '" (format "%S" package-or-recipe) ")") nil deferred-file))
        (autoload function deferred-file nil interactive)))))

;; -----------------------------------------------------------------------------
;; Configuration
;; -----------------------------------------------------------------------------

(setq custom-file (expand-file-name "custom.el" emacs-dir))

;; tex-site is needed at several places?
(require-package 'auctex 'tex-site)

(require-package 'org-mode)

(defun org-babel-add-to-load-path (dir)
  (let ((age (lambda (file)
               (float-time
                (time-subtract (current-time)
                               (nth 5 (or (file-attributes (file-truename file))
                                          (file-attributes file)))))))
        (files (directory-files dir nil "\\.org$")))
    (require 'ob-core)
    (while files
      (let* ((file (expand-file-name (car files) dir))
             (base-name (file-name-sans-extension file))
             (exported-file (concat base-name ".el")))
        ;; tangle if the org-mode file is newer than the elisp file
        (unless (and (file-exists-p exported-file)
                     (> (funcall age file) (funcall age exported-file)))
          (org-babel-tangle-file file exported-file "emacs-lisp")))
      (setq files (cdr files))))
  (add-to-list 'load-path dir))

(org-babel-add-to-load-path config-dir)

(require 'config-misc)
(require 'config-appearance)
(require 'config-whitespace)
(require 'config-completion)
(require 'config-bindings)
(require 'config-c)
(require 'config-clojure)
(require 'config-css)
(require 'config-git)
(require 'config-im)
(require 'config-java)
(require 'config-js)
(require 'config-latex)
(require 'config-lisp)
(require 'config-markdown)
(require 'config-python)
(require 'config-rtf)
(require 'config-scala)
(require 'config-shell)
(require 'config-xml)
