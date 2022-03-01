;;; my-kubernetes.el --- kubernetes configuration.

;;; Commentary:

;; Basic configuration for kubernetes.

;;; Code:

(use-package kubernetes
  :defer t

  :init
  (defalias 'ko 'kubernetes-overview))

(provide 'my-kubernetes)
;;; my-kubernetes.el ends here
