;;; my-text-mode.el - -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for text-mode.

;;; Code:

(use-package text-mode
  :straight (:type built-in)
  :defer t
  :hook (text-mode . my/text-mode-setup)
  :init
  (defun my/text-mode-setup ()
    (setq-local fill-column 80)

    (hl-line-mode t)
    (visual-line-mode t)))

(provide 'my-text-mode)
;;; my-text-mode.el ends here
