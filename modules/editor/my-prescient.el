;;; my-prescient.el --- prescient configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for prescient.

;;; Code:

(use-package prescient
  :defer t

  :custom
  (prescient-filter-method '(literal-prefix literal regexp initialism fuzzy))
  (prescient-history-length 100)
  (prescient-save-file (expand-file-name "prescient-save.el" my/cache-dir))
  (prescient-sort-full-matches-first t)
  (prescient-sort-length-enable nil)

  :config
  (prescient-persist-mode +1))

(provide 'my-prescient)
;;; my-prescient.el ends here
