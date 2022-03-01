;;; my-helm-lsp.el --- helm-lsp configuration.

;;; Commentary:

;; Basic configuration for helm-lsp.

;;; Code:

(require 'my-helm)
(require 'my-lsp)

(use-package helm-lsp
  :after (helm-global-bindings)
  :bind
  ("C-c '" . helm-lsp-workspace-symbol)
  ("C-c C-'" . helm-lsp-workspace-symbol)
  ("C-c \\" . helm-lsp-diagnostics))

(provide 'my-helm-lsp)
;;; my-helm-lsp.el ends here
