;;; my-yasnippet.el --- yasnippet configuration.

;;; Commentary:

;; Basic configuration for yasnippet.

;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (emacs-startup . yas-reload-all)

  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'my-yasnippet)
;;; my-yasnippet.el ends here
