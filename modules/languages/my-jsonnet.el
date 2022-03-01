;;; my-jsonnet.el --- jsonnet-mode configuration.

;;; Commentary:

;; Basic configuration for jsonnet-mode.

;;; Code:

(use-package jsonnet-mode
  :bind (:map jsonnet-mode-map
              ("C-c C-c" . jsonnet-eval-buffer)
              ("C-c C-f" . jsonnet-reformat-buffer)
              ("C-c C-j" . jsonnet-jump))

  :hook
  (jsonnet-mode . my/jsonnet-mode-setup)

  :custom
  (jsonnet-library-search-directories '("vendor"))

  :init
  (defun my/jsonnet-mode-setup ()
    "Default tweaks for `jsonnet-mode'."
    (jsonnet-format-buffer-on-save-mode t)
    (company-mode)
    (subword-mode)
    (my/folding))

  :config
  (with-eval-after-load 'flycheck
    (setq flycheck-jsonnet-executable "jsonnet -jpath vendor"))

  (define-minor-mode jsonnet-format-buffer-on-save-mode
    "Run jsonnet-format-buffer as a before-save-hook."
    :lighter " fmt"
    (if jsonnet-format-buffer-on-save-mode
        (add-hook 'before-save-hook 'jsonnet-reformat-buffer t t)
      (remove-hook 'before-save-hook 'jsonnet-reformat-buffer t))))

(provide 'my-jsonnet)
;;; my-js.el ends here
