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

(load (concat config-dir "macosx.el"))
(load (concat config-dir "general.el"))
(load (concat config-dir "appearance.el"))
(load (concat config-dir "whitespace.el"))
(load (concat config-dir "completion.el"))

(load (concat config-dir "c.el"))
(load (concat config-dir "clojure.el"))
(load (concat config-dir "css.el"))
(load (concat config-dir "git.el"))
(load (concat config-dir "irc.el"))
(load (concat config-dir "jabber.el"))
(load (concat config-dir "java.el"))
(load (concat config-dir "js.el"))
(load (concat config-dir "latex.el"))
(load (concat config-dir "lisp.el"))
(load (concat config-dir "markdown.el"))
(load (concat config-dir "python.el"))
(load (concat config-dir "rtf.el"))
(load (concat config-dir "scala.el"))
(load (concat config-dir "shell.el"))
(load (concat config-dir "xml.el"))
