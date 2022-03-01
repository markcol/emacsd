;;; my-pandoc.el --- pandoc-mode configuration.

;;; Commentary:

;; Basic configuration for pandoc-mode.

;;; Code:

(use-package pandoc-mode
  :hook
  (markdown-mode . pandoc-mode))

(provide 'my-pandoc)
;;; my-pandoc.el ends here
