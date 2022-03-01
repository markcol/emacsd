;;; my-ob-mermaid.el --- ob-mermaid configuration.

;;; Commentary:

;; Basic configuration for ob-mermaid.

;;; Code:

(require 'my-org-mode)

(use-package ob-mermaid
  :defer t

  :hook
  (org-mode . my/ob-mermaid-setup)

  :init
  (defun my/ob-mermaid-setup ()
    (require 'ob-mermaid)))

(provide 'my-ob-mermaid)
;;; my-ob-mermaid.el ends here
