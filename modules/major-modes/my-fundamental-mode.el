;;; my-fundamental-mode.el - -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for fundamental-mode.

;;; Code:

(use-package fundamental-mode
  :straight (:type built-in)
  :defer t

  :hook
  (fundamental-mode . my/fundamental-mode-setup)

  :init
  (defun my/fundamental-mode-setup ()
    (hl-line-mode t)))

(provide 'my-fundamental-mode)
;;; my-fundamental-mode.el ends here
