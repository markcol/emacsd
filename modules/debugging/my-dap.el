;;; my-dap.el --- dap-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for dap-mode.

;;; Code:

(require 'my-debug-map)
(require 'my-hydra)

(use-package dap-mode
  :defer t

  :bind (:map my/debug-map
              ("d" . dap-debug)
              ("t" . dap-breakpoint-toggle)
              ("c" . dap-breakpoint-condition)
              ("h" . dap-breakpoint-hit-condition)
              ("m" . dap-breakpoint-log-message)
              ("l" . dap-ui-breakpoints-list)
              ("b" . dap-ui-breakpoints))

  :custom
  (dap-auto-configure-features '(sessions locals controls tooltip))

  :config
  (dap-auto-configure-mode t)
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

(provide 'my-dap)
;;; my-dap.el ends here
