;;; my-makefile.el --- makefile-mode configuration.

;;; Commentary:

;; Basic configuration for makefile-mode.

;;; Code:

(require 'my-makefile-executor)

(use-package make-mode
  :straight (:type built-in)
  :hook
  (makefile-mode . my/makefile-mode-setup)

  :bind
  (:map makefile-mode-map
        ("C-c C-m" . makefile-executor-execute-project-target))

  :init
  (add-to-list 'my/indent-sensitive-modes 'makefile-mode)

  (defun my/makefile-mode-setup ()
    (subword-mode +1)
    (my/display-indentation -1)
    (setq tab-width 4)))

(provide 'my-makefile)
;;; my-makefile.el ends here
