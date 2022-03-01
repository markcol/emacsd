;;; my-sass.el --- sass-mode configuration.

;;; Commentary:

;; Basic configuration for sass-mode.

;;; Code:

(require 'my-css)

(use-package sass-mode
  :mode "\\.sass\\'"

  :hook
  (sass-mode . my/sass-mode-setup)

  :custom
  ;; Turn off annoying auto-compile on save.
  (sass-compile-at-save nil)

  :init
  (defun my/sass-mode-setup ()
    (my/css-mode-css)))

(provide 'my-sass)
;;; my-sass.el ends here
