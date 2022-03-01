;;; my-jsx.el --- .jsx file configuration

;;; Commentary:

;; Basic configuration for dealing with .jsx files.

;;; Code:

(require 'my-folding)
(require 'my-prettier-js)

(use-package rjsx-mode
  :mode "components\\/.*\\.js\\'"

  :hook (rjsx-mode . my/rjsx-mode-setup)

  :init
  (defun my/rjsx-mode-setup ()
    (prettier-js-mode +1)
    (company-mode +1)
    (subword-mode +1)
    (my/folding)))

(provide 'my-jsx)
;;; my-jsx.el ends here
