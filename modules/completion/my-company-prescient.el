;;; my-company-prescient.el --- company-prescient configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for company-prescient.

;;; Code:

(require 'my-company)
(require 'my-prescient)

(use-package company-prescient
  :defer t
  :requires (prescient company)

  :config
  (company-prescient-mode +1))

(provide 'my-company-prescient)
;;; my-company-prescient.el ends here
