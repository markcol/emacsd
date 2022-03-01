;;; my-packages.el --- avy configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for packages.

;;; Code:

(require 'my-flycheck)

(use-package package-lint
  :defer t)

(use-package package-build
  :defer t)

(use-package flycheck-package
  :defer t
  :config
  (flycheck-package-setup))

(provide 'my-packages)
;;; my-packages.el ends here
