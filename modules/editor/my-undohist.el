;;; my-undohist.el --- undohist configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for undohist.

;;; Code:

(use-package undohist
  :demand
  :custom
  (undohist-directory (expand-file-name "undohist" my/cache-dir))
  (undohist-ignored-files '("COMMIT_EDITMSG"))

  :config
  (undohist-initialize))

(provide 'my-undohist)
;;; my-undohist.el ends here
