;;; my-json.el --- json-mode configuration.

;;; Commentary:

;; Basic configuration for json-mode.

;;; Code:

(require 'my-folding)
(require 'my-js)
(require 'my-lsp)

(use-package json-mode
  :mode "\\.json\\'"

  :bind (:map json-mode-map
              ("C-j" . newline-and-indent)
              ("C-c C-h" . my/folding-toggle))

  :hook
  (json-mode . my/json-mode-setup)

  :init
  (defun my/json-mode-setup ()
    "Default tweaks for `json-mode'."

    (let ((width 2))
      (setq js-indent-level width
            json-reformat:indent-width width
            tab-width width))))

(use-package lsp-json
  :straight lsp-mode

  :hook
  (json-mode . my/lsp-json-mode-setup)

  :init
  (defun my/lsp-json-mode-setup ()
    (lsp-deferred)
    (lsp-format-buffer-on-save-mode)))

(provide 'my-json)
;;; my-js.el ends here
