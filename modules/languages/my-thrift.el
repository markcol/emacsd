;;; my-thrift.el --- thrift-mode configuration.

;;; Commentary:

;; Basic configuration for thrift-mode.

;;; Code:

(require 'my-prog-mode)

(use-package thrift
  :mode "\\.thrift\\'"
  :hook (thrift-mode . my/thrift-mode-setup)

  :init
  (defun my/thrift-mode-setup ()
    (run-hooks 'prog-mode-hook)
    (setq tab-width 2)

    (subword-mode +1)))

(provide 'my-thrift)
;;; my-thrift.el ends here
