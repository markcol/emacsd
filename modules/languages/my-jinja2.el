;;; my-jinja2.el --- jinja2-mode configuration.

;;; Commentary:

;; Basic configuration for jinja2-mode.

;;; Code:

(require 'my-prog-mode)

(use-package jinja2-mode
  :hook
  (jinja2-mode . my/jinja2-mode-setup)

  :init
  (defun my/jinja2-mode-setup ()
    (run-hooks 'prog-mode-hook)
    (subword-mode +1)))

(provide 'my-jinja2)
;;; my-jinja2.el ends here
