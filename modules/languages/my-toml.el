;;; my-toml.el --- toml-mode configuration.

;;; Commentary:

;; Basic configuration for toml-mode.

;;; Code:

(require 'my-prettier-js)
(require 'my-prog-mode)

(use-package conf-toml-mode
  :straight (:type built-in)
  :mode "\\.toml\\'"
  :hook (conf-toml-mode . my/toml-mode-setup)

  :init
  (defun my/toml-mode-setup ()
    (run-hooks 'prog-mode-hook)
    (setq tab-width 2)
    (prettier-js-mode)))

(provide 'my-toml)
;;; my-toml.el ends here
