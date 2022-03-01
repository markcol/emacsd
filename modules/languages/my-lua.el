;;; my-lua.el --- lua-mode configuration.

;;; Commentary:

;; Basic configuration for lua-mode.

;;; Code:

(use-package lua-mode
  :hook
  (lua-mode . my/lua-mode-setup)

  :init
  (defun my/lua-mode-setup ()
    (setq lua-indent-level 2
          whitespace-action '(auto-cleanup))

    (subword-mode +1)))

(use-package lsp-lua
  :straight lsp-mode

  :hook
  (lua-mode . my/lsp-lua-mode-setup)

  :custom
  (lsp-lua-hint-enable t)
  (lsp-lua-telemetry-enable nil)

  :init
  (defun my/lsp-lua-mode-setup ()
    (lsp-deferred)))

(provide 'my-lua)
;;; my-lua.el ends here
