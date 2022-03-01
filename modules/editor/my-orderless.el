;;; my-orderless.el --- orderless configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for orderless.

;;; Code:

(use-package orderless
  :defines (orderlies-matching-styles)
  :commands (orderless-filter orderless-highlight-matches)
  :custom
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (orderless-matching-styles '(orderless-literal
                               orderless-prefixes
                               orderless-regexp
                               orderless-initialism
                               orderless-flex))

  :init
  (with-eval-after-load 'selectrum
    (setq selectrum-refine-candidates-function #'orderless-filter)
    (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)))

(provide 'my-orderless)
;;; my-orderless.el ends here
