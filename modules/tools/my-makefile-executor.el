;;; my-makefile-executor.el --- makefile-executor configuration.

;;; Commentary:

;; Basic configuration for makefile-executor.

;;; Code:

(require 'helm-make)

(use-package makefile-executor
  :hook
  (makefile-mode . makefile-executor-mode)

  :bind
  ("C-c C-m" . makefile-executor-execute-project-target)

  :config
  ;; Replace get targets function with that from helm-make as it faster and more
  ;; efficient by not needing to write any temporary files to disk.
  (defalias 'makefile-executor-get-targets 'helm--make-target-list-qp))

(provide 'my-makefile-executor)
;;; my-makefile-executor.el ends here
