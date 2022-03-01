;;; my-helm-make.el --- helm-make configuration.

;;; Commentary:

;; Basic configuration for helm-make.

;;; Code:

(require 'my-helm)

(use-package helm-make
  :after (helm-global-bindings)
  :bind
  (:map helm-command-map
        ("m" . helm-make-projectile))

  :custom
  (helm-make-cache-targets nil)
  (helm-make-do-save t)
  (helm-make-fuzzy-matching t)
  (helm-make-list-target-method 'qp))

(provide 'my-helm-make)
;;; my-helm-make.el ends here
