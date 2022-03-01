;;; my-special-mode.el - -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for special-mode.

;;; Code:

(use-package special-mode
  :straight (:type built-in)
  :defer t

  :hook
  (special-mode . my/special-mode-setup)

  :init
  (defun my/special-mode-setup ()
    (hl-line-mode t)))

(provide 'my-special-mode)
;;; my-special-mode.el ends here
