;;; my-github-review.el --- github-review configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for github-review.

;;; Code:

(use-package github-review
  :defer t
  :custom
  (github-review-fill-column 80)
  (github-review-lgtm-message "lgtm :)")
  (github-review-new-buffer-window-strategy 'switch-to-buffer))

(provide 'my-github-review)
;;; my-github-review.el ends here
