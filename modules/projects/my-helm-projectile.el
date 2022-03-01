;;; my-helm-projectile.el - -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for helm-projectile.

;;; Code:

(require 'my-helm)
(require 'my-projectile)

(use-package helm-projectile
  :after (helm-global-bindings)
  :bind
  ("C-x ;"   . helm-projectile-find-file)
  ("C-x C-;" . helm-projectile-find-file)
  ("C-c ;"   . helm-projectile-switch-project)
  ("C-c C-;" . helm-projectile-switch-project))

(provide 'my-helm-projectile)
;;; my-helm-projectile.el ends here
