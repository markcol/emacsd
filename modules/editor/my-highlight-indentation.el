;;; my-highlight-indentation.el --- highlight-indentation-mode configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for highlight-indentation-mode.

;;; Code:

(use-package highlight-indentation
  :defer t
  :diminish (highlight-indentation-mode
             highlight-indentation-current-column-mode))

(provide 'my-highlight-indentation)
;;; my-highlight-indentation.el ends here
