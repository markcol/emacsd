;;; my-ob-go.el --- ob-go configuration.

;;; Commentary:

;; Basic configuration for ob-go.

;;; Code:

(require 'my-org-mode)

(use-package ob-go
  :defer t

  :hook
  (org-mode . my/ob-go-setup)

  :init
  (defun my/ob-go-setup ()
    (require 'ob-go)))

(provide 'my-ob-go)
;;; my-ob-go.el ends here
