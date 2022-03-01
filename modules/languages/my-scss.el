;;; my-scss.el --- scss-mode configuration.

;;; Commentary:

;; Basic configuration for scss-mode.

;;; Code:

(require 'my-css)

(use-package scss-mode
  :mode "\\.scss\\'"

  :hook
  (scss-mode . my/scss-mode-setup)

  :custom
  ;; Turn off annoying auto-compile on save.
  (scss-compile-at-save nil)

  :init
  (defun my/scss-mode-setup ()
    (my/css-mode-setup)))

(provide 'my-scss)
;;; my-scss.el ends here
