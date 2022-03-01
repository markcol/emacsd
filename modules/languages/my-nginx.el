;;; my-nginx.el --- nginx-mode configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for nginx-mode.

;;; Code:

(use-package nginx-mode
  :hook
  (nginx-mode . my/nginx-mode-setup)

  :custom
  (nginx-indent-level 4)
  (nginx-indent-tabs-mode nil)

  :preface
  (defun my/nginx-mode-setup ()
    (setq tab-width 4)))

(use-package company-nginx
  ;; Override company-nginx recipe as the original repo was
  ;; deleted.
  :straight (:type git :host github :repo "emacsmirror/company-nginx" :branch "master")

  :requires (coompany nginx-mode)
  :commands (company-nginx-keywords)
  :hook
  (nginx-mode . company-nginx-keywords))

(provide 'my-nginx)
;;; my-nginx.el ends here
