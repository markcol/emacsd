;;; my-yaml.el - -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for yaml-mode.

;;; Code:

(require 'my-lsp)
(require 'my-prog-mode)

(use-package yaml-mode
  :mode "\\.yml\\'" "\\.yaml\\'"
  :bind (:map yaml-mode-map
              ("RET" . newline-and-indent))

  :hook
  (yaml-mode . my/yaml-mode-setup)

  :init
  (defun my/yaml-mode-setup ()
    (run-hooks 'prog-mode-hook)

    (setq tab-width 2)
    (subword-mode +1)))

(use-package lsp-yaml
  :straight lsp-mode

  :hook
  (yaml-mode . my/lsp-yaml-mode-setup)

  :init
  (defun my/lsp-yaml-mode-setup ()
    (lsp-deferred)
    (lsp-format-buffer-on-save-mode)))

(use-package yaml-imenu
  :after yaml-mode
  :config
  (yaml-imenu-enable))

(provide 'my-yaml)
;;; my-yaml.el ends here
