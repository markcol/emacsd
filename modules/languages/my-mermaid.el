;;; my-mermaid.el --- mermaid-mode configuration.

;;; Commentary:

;; Basic configuration for mermaid-mode.

;;; Code:

(require 'my-prog-mode)

(use-package mermaid-mode
  :mode "\\.mermaid\\'" "\\.mmd\\'"

  :hook
  (mermaid-mode . my/mermaid-mode-setup)

  :init
  (defun my/mermaid-mode-setup ()
    (run-hooks 'prog-mode-hook)))

(provide 'my-mermaid)
;;; my-mermaid.el ends here
