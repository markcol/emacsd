;;; my-prettier-js.el --- prettier-js configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for prettier-js package

;;; Code:

(use-package prettier-js
  :defer t
  :hook (prettier-js-mode . my/prettier-js-mode-setup)

  :preface
  (defun my/prettier-js-mode-setup ()))

(provide 'my-prettier-js)
;;; my-prettier-js.el ends here
