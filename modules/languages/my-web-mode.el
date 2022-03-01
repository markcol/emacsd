;;; my-web-mode.el --- web-mode configuration.

;;; Commentary:

;; Basic configuration for web-mode.

;;; Code:

(require 'my-company)
(require 'my-display-fill-column)
(require 'my-folding)

(use-package web-mode
  :mode
  "\\.html\\'"
  "\\.html.erb\\'"
  "\\.tpl\\'"

  :bind (:map web-mode-map
              ("C-j" . newline-and-indent)
              ("C-c C-h" . my/folding-toggle))

  :hook
  (web-mode . my/web-mode-setup)

  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-sql-indent-offset 2)
  (web-mode-engines-alist '(("go" . "\\.tpl\\'")))

  :init
  (defun my/web-mode-setup ()
    "Default tweaks for `web-mode'."
    (setq tab-width 2)

    (when (version< emacs-version "27.0")
      (my/display-fill-column -1))

    (company-mode +1)
    (my/folding)
    (subword-mode +1)
    (lsp-deferred)))

(provide 'my-web-mode)
;;; my-web-mode.el ends here
