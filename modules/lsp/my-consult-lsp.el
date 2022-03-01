;;; my-consult-lsp.el --- consult-lsp configuration.

;;; Commentary:

;; Basic configuration for consult-lsp.

;;; Code:

(require 'my-consult)
(require 'my-lsp)

(use-package consult-lsp
  :bind
  ("C-c C-\\" . consult-lsp-diagnostics))

(provide 'my-consult-lsp)
;;; my-consult-lsp.el ends here
