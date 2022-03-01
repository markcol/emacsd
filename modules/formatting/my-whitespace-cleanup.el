;;; my-whitespace-cleanup.el --- whitespace-cleanup-mode configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for whitespace-cleanup-mode package

;;; Code:

(use-package whitespace-cleanup-mode
  :defer t
  :hook
  (prog-mode . whitespace-cleanup-mode)

  :custom
  (whitespace-cleanup-mode-preserve-point nil)
  (whitespace-cleanup-mode-only-if-initially-clean nil))

(provide 'my-whitespace-cleanup)
;;; my-whitespace-cleanup.el ends here
