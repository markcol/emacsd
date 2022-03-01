;;; my-groovy.el --- groovy-mode configuration.

;;; Commentary:

;; Basic configuration for groovy-mode.

;;; Code:

(use-package groovy-mode
  :mode "\\.groovy\\'"
  :hook
  (groovy-mode . my/groovy-mode-setup)

  :init
  (defun my/groovy-mode-setup ()
    (setq groovy-highlight-assignments t
          groovy-indent-offset 4
          tab-width 4)

    (subword-mode +1)))

(provide 'my-groovy)
;;; my-groovy.el ends here
