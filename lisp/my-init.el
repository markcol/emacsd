;;; my-init.el --- init file.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq load-prefer-newer t)

(defvar my/lisp-dir (file-name-directory load-file-name)
  "Core directory within Emacs configuration.")

(add-to-list 'load-path my/lisp-dir)

(defvar my/cache-dir (expand-file-name "cache" user-emacs-directory)
  "Cache directory within Emacs configuration.")

(unless (file-exists-p my/cache-dir)
  (make-directory my/cache-dir))

;; My customizations
(require 'my-custom)
(require 'my-utils)
(require 'my-packages)
(require 'my-performance)
(require 'my-env)
(require 'my-ui)
(require 'my-editor)

;; macOS specific
(when (eq system-type 'darwin)
  (require 'my-macos))

;; Config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

(require 'my-themes)
(require 'my-modules)

(provide 'my-init)
;;; my-init.el ends here
