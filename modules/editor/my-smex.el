;;; my-smex.el --- smex configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Replace M-x with the more powerful smex.

;;; Code:

(use-package smex
  :bind
  ("C-x C-m" . smex)
  ("C-c C-m" . smex)
  ("M-X" . smex-major-mode-commands)
  ("C-c C-c M-x" . execute-extended-command)

  :custom
  (smex-save-file (expand-file-name "smex-items" my/cache-dir))

  :config
  (smex-initialize))

(provide 'my-smex)
;;; my-smex.el ends here
