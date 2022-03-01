;;; my-haml.el --- haml-mode configuration.

;;; Commentary:

;; Basic configuration for haml-mode.

;;; Code:

(use-package haml-mode
  :mode "\\.haml\\'" "\\.hamlc\\'"
  :hook (haml-mode . my/haml-mode-setup)

  :init
  (defun my/haml-mode-setup ()
    (setq tab-width 2)))

(provide 'my-haml)
;;; my-haml.el ends here
