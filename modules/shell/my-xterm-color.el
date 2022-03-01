;;; my-xterm-color.el --- xterm-color configuration.

;;; Commentary:

;; Basic configuration for xterm-color.

;;; Code:

(use-package xterm-color
  :demand t
  :custom
  (compilation-environment '("TERM=xterm-256color"))

  :init
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))

  :config
  (advice-add 'compilation-filter :around #'my/advice-compilation-filter))

(provide 'my-xterm-color)
;;; my-xterm-color.el ends here
