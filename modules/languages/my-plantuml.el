;;; my-plantuml.el --- plantuml-mode configuration.

;;; Commentary:

;; Basic configuration for plantuml-mode.

;;; Code:

(use-package plantuml-mode
  :mode "\\.uml\\'"
  :hook
  (plantuml-mode . my/plantuml-mode-setup)

  :custom
  (plantuml-default-exec-mode 'executable)

  :init
  (defun my/plantuml-mode-setup ()
    (setq tab-width 2)))

(provide 'my-plantuml)
;;; my-plantuml.el ends here
