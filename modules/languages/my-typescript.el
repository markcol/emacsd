;;; my-typescript.el --- typescript-mode configuration.

;;; Commentary:

;; Basic configuration for typescript-mode.

;;; Code:

(require 'my-company)
(require 'my-flycheck)
(require 'my-folding)
(require 'my-lsp)
(require 'my-prettier-js)
(require 'my-web-mode)

(use-package typescript-mode
  :defer t
  :mode "\\.ts\\'"
  :hook
  (typescript-mode . my/typescript-mode-setup)

  :bind (:map typescript-mode-map
              ("C-j" . newline-and-indent)
              ("C-c C-h" . my/folding-toggle))

  :init
  (defun my/typescript-mode-setup ()
    (let ((width 2))
      (setq typescript-indent-level width
            indent-level width
            tab-width width))

    (company-mode +1)
    (lsp-deferred)
    (subword-mode +1)
    (my/folding)))

(use-package tide
  :hook
  (typescript-mode . my/tide-mode-setup)
  (web-mode . my/tide-web-mode-setup)

  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'typescript-tslint 'web-mode))

  (defun my/tide-web-mode-setup ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (my/tide-mode-setup)))

  (defun my/tide-mode-setup ()
    (interactive)
    (tide-setup)

    (setq flycheck-check-syntax-automatically '(save mode-enabled)
          company-tooltip-align-annotations t)

    (prettier-js-mode +1)
    (flycheck-mode +1)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)))

(provide 'my-typescript)
;;; my-typescript.el ends here
