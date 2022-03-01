;;; my-lorem-ipsum.el --- lorem-ipsum configuration. -*- lexical-binding: t ; -*-

;;; Commentary:

;; Basic configuration for lorem-ipsum.

;;; Code:

(use-package lorem-ipsum
  :bind
  ("C-c l s" . lorem-ipsum-insert-sentences)
  ("C-c l p" . lorem-ipsum-insert-paragraphs)
  ("C-c l l" . lorem-ipsum-insert-list))

(provide 'my-lorem-ipsum)
;;; my-lorem-ipsum.el ends here
