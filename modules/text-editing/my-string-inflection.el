;;; my-string-inflection.el - -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for string-inflections.

;;; Code:

(use-package string-inflection
  :bind
  ("C-c C-u" . string-inflection-cycle))

(provide 'my-string-inflection)
;;; my-string-inflection.el ends here
