;;; my-ssh-config.el --- ssh-config-mode configuration.

;;; Commentary:

;; Basic configuration for ssh-config-mode.

;;; Code:

(require 'my-prog-mode)

(use-package ssh-config-mode
  :mode "/ssh/config\\'"
  :hook (ssh-config-mode . my/ssh-config-mode-setup)

  :init
  (defun my/ssh-config-mode-setup ()
    (run-hooks 'prog-mode-hook)
    (setq tab-width 2)))

(provide 'my-ssh-config)
;;; my-ssh-config.el ends here
