;;; my-xwwp.el --- xwwp configuration.

;;; Commentary:

;; Basic configuration for xwwp.

;;; Code:

(use-package xwwp
  :bind
  (:map xwidget-webkit-mode-map
        ("v" . xwwp-follow-link)))

(provide 'my-xwwp)
;;; my-xwwp.el ends here
