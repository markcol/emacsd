;;; my-code-review.el --- code-review configuration

;;; Commentary:

;; Basic configuration for code-review.

;;; Code:

(use-package code-review
  :defer t
  :custom
  (code-review-db-database-file (expand-file-name
                                 "code-review-db.sqlite" my/cache-dir))
  (code-review-fill-column 80)
  (code-review-lgtm-message "lgtm :)")
  (code-review-new-buffer-window-strategy 'switch-to-buffer))

(provide 'my-code-review)
;;; my-code-review.el ends here
