;;; my-rust.el --- rust-mode configuration.

;;; Commentary:

;; Basic configuration for rust-mode.

;;; Code:

(require 'my-company)
(require 'my-folding)
(require 'my-lsp)

(use-package rust-mode
  :mode "\\.rs\\'"
  :interpreter "rust"
  :commands rust-mode
  :bind (:map rust-mode-map
              ("RET" . newline-and-indent))

  :hook
  (rust-mode . my/rust-mode-setup)

  :init
  (defun my/rust-mode-setup ()
    (setq rust-format-on-save t)

    (company-mode +1)
    (lsp-deferred)
    (my/folding)
    (subword-mode +1)))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package rust-playground
  :defer t)

(provide 'my-rust)
;;; my-rust.el ends here
