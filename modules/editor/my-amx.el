;;; my-amx.el --- amx configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Replace M-x with the more powerful amx.

;;; Code:

(require 'my-ido)

(use-package amx
  :bind
  ("M-x"     . amx)
  ("C-x C-m" . amx)

  :custom
  (amx-backend 'ido)
  (amx-histroy-lenth 15)
  (amx-prompt-string "M-x ")
  (amx-save-file (expand-file-name "amx-items" my/cache-dir))
  (amx-show-key-bindings t)

  :config
  (amx-mode +1))

(provide 'my-amx)
;;; my-amx.el ends here
