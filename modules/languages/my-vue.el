;;; my-vue.el --- vue-mode configuration.

;;; Commentary:

;; Basic configuration for vue-mode.

;;; Code:

(require 'my-company)
(require 'my-prettier-js)
(require 'my-mmm)

(use-package vue-mode
  :hook
  (vue-mode . my/vue-mode-setup)

  :init
  (defun my/vue-mode-setup ()
    (prettier-js-mode)
    (company-mode)
    (lsp-deferred)
    (subword-mode)
    (my/folding)))

(provide 'my-vue)
;;; my-vue.el ends here
