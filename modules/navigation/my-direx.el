;;; my-direx.el --- direx configuration.

;;; Commentary:

;; Basic configuration for direx.

;;; Code:

(use-package direx
  :bind ("C-x j" . direx-project:jump-to-project-root)
  :hook
  (direx-mode . my/direx-mode-setup)

  :custom
  (direx:closed-icon " + ")
  (direx:open-icon " - ")

  :init
  (defun my/direx-mode-setup ()))

(provide 'my-direx)
;;; my-direx.el ends here
