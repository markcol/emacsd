;;; my-selectrum-prescient.el --- selectrum-prescient configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for selectrum-prescient.

;;; Code:

(use-package selectrum-prescient
  :after (selectrum prescient)

  :config
  (selectrum-prescient-mode +1))

(provide 'my-selectrum-prescient)
;;; my-selectrum-prescient.el ends here
