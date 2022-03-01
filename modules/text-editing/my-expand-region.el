;;; my-expand-region.el --- expand-region configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Configuration for expand-region

;;; Code:

(use-package expand-region
  :bind
  ("M-." . er/expand-region)
  ("M-," . er/contract-region))

(provide 'my-expand-region)
;;; my-expand-region.el ends here
