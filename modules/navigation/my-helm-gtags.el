;;; my-helm-gtags.el --- helm-gtags configuration.

;;; Commentary:

;; Basic configuration for helm-gtags.

;;; Code:

(require 'my-helm)

(use-package helm-gtags
  :defer t
  :after (helm-global-bindings)

  :custom
  (helm-gtags-auto-update t)
  (helm-gtags-direct-helm-completing t)
  (helm-gtags-fuzzy-match t)
  (helm-gtags-ignore-case t))

(provide 'my-helm-gtags)
;;; my-helm-gtags.el ends here
