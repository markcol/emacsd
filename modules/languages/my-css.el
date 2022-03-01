;;; my-css.el --- css-mode configuration.

;;; Commentary:

;; Basic configuration for css-mode.

;;; Code:

(require 'my-company)
(require 'my-lsp)
(require 'my-prettier-js)
(require 'my-rainbow)

(use-package css-mode
  :mode "\\.css\\'"

  :hook
  (css-mode . my/css-mode-setup)

  :custom
  (css-indent-offset 2)

  :init
  (defun my/css-mode-setup ()
    (setq tab-width 2)

    (company-mode +1)
    (prettier-js-mode)
    (lsp-deferred)
    (rainbow-mode +1)))

(provide 'my-css)
;;; my-css.el ends here
