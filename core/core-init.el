;;; core-init.el --- init file.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq load-prefer-newer t)

(defvar my/core-dir (file-name-directory load-file-name)
  "Core directory within Emacs configuration.")

(add-to-list 'load-path my/core-dir)

(defvar my/cache-dir (expand-file-name "cache" user-emacs-directory)
  "Cache directory within Emacs configuration.")

(unless (file-exists-p my/cache-dir)
  (make-directory my/cache-dir))

;; Core stuff
(require 'core-custom)
(require 'core-utils)

;; Continue core stuff
(require 'core-packages)
(require 'core-performance)
(require 'core-env)
(require 'core-ui)
(require 'core-editor)

;; macOS specific
(when (eq system-type 'darwin)
  (require 'core-macos))

;; Config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

(require 'core-themes)

(require 'core-modules)

(provide 'core-init)
;;; core-init.el ends here
