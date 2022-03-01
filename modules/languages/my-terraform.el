;;; my-terraform.el --- terraform-mode configuration.

;;; Commentary:

;; Basic configuration for terraform-mode.

;;; Code:

(require 'my-company)
(require 'my-reformatter)
(require 'my-lsp)

(use-package terraform-mode
  :hook
  (terraform-mode . my/terraform-mode-setup)

  :custom
  (terraform-indent-level 2)

  :init
  (defun my/terraform-mode-setup ()
    (setq-local tab-width 2)
    (terraform-format-on-save-mode 1)
    (lsp-deferred))

  :config
  ;; This does a better job of injecting formatted content than the default
  ;; formatting commands included with terraform-mode.
  (reformatter-define terraform-format
    :program "terraform"
    :args '("fmt" "-no-color" "-")
    :lighter " TF"))

(use-package terraform-doc
  :defer t)

(provide 'my-terraform)
;;; my-terraform.el ends here
