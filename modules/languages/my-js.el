;;; my-js.el --- js-mode configuration.

;;; Commentary:

;; Basic configuration for js-mode.

;;; Code:

(require 'my-company)
(require 'my-folding)
(require 'my-lsp)

(use-package js-mode
  :straight (:type built-in)
  :mode
  "\\.js\\'"
  "\\.pac\\'"

  :bind (:map js-mode-map
              ("C-j" . newline-and-indent)
              ("C-c C-h" . my/toggle-hiding))

  :hook
  (js-mode . my/js-mode-setup)

  :init
  (defun my/js-mode-setup ()
    "Default tweaks for `js-mode'."
    (let ((width 2))
      (setq js-indent-level width
            indent-level width
            tab-width width))

    (company-mode)
    (subword-mode)
    (my/folding)))

(use-package lsp-javascript
  :straight lsp-mode

  :hook
  (js-mode . my/lsp-js-mode-setup)

  :init
  (defun my/lsp-js-mode-setup ()
    (lsp-deferred)
    (lsp-format-buffer-on-save-mode)))

(provide 'my-js)
;;; my-js.el ends here
