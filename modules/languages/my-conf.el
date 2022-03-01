;;; my-conf.el --- conf-mode configuration.

;;; Commentary:

;; Basic configuration for conf-mode.

;;; Code:

(require 'my-prog-mode)

(use-package conf-mode
  :straight (:type built-in)
  :mode
  "/Procfile\\'"
  "/\\.env\\'"
  "/\\.env\\.[^/]+\\'"
  "\\.cfg\\'"
  "\\.conf\\'"

  :hook (conf-mode . my/conf-mode-setup)

  :init
  (defun my/conf-mode-setup ()
    (run-hooks 'prog-mode-hook)
    (setq tab-width 2)))

(provide 'my-conf)
;;; my-conf.el ends here
