;;; my-imenu.el --- imenu configuration.

;;; Commentary:

;; Basic configuration for imenu.

;;; Code:

(require 'imenu)
(require 'my-helm)

(use-package imenu
  :straight (:type built-in)
  :defer t

  :custom
  (imenu-auto-rescan t)
  (imenu-max-item-length 160)
  (imenu-max-items 400))

(use-package imenu-anywhere
  :bind
  ("C-c t" . helm-imenu-anywhere))

(provide 'my-imenu)
;;; my-imenu.el ends here
