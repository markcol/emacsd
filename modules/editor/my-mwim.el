;;; my-mwim.el --- mwim configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for mwim.

;;; Code:

(use-package mwim
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))

(provide 'my-mwim)
;;; my-mwim.el ends here
