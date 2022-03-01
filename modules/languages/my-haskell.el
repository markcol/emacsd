;;; my-haskell.el --- haskell-mode configuration.

;;; Commentary:

;; Basic configuration for haskell-mode.

;;; Code:

(require 'my-company)
(require 'my-folding)

(use-package haskell-mode
  :bind (:map haskell-mode-map
              ("RET" . newline-and-indent))

  :hook
  (haskell-mode . my/haskell-mode-setup)

  :init
  (defun my/haskell-mode-setup ()
    (company-mode +1)
    (my/folding)
    (subword-mode +1)))

(provide 'my-haskell)
;;; my-haskell.el ends here
