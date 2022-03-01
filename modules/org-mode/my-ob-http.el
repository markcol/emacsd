;;; my-ob-http.el --- ob-http configuration.

;;; Commentary:

;; Basic configuration for ob-http.

;;; Code:

(require 'my-org-mode)

(use-package ob-http
  :defer t

  :hook
  (org-mode . my/ob-http-setup)

  :init
  (defun my/ob-http-setup ()
    (require 'ob-http)))

(provide 'my-ob-http)
;;; my-ob-http.el ends here
