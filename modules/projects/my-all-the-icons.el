;;; my-all-the-icons.el --- all-the-icons configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for all-the-icons.

;;; Code:

;; Required by all-the-icons
(use-package memoize)

(use-package all-the-icons
  :after (memoize)
  :if window-system)

(use-package all-the-icons-ibuffer
  :after (all-the-icons)
  :if window-system
  :init (all-the-icons-ibuffer-mode 1))

(provide 'my-all-the-icons)
;;; my-all-the-icons.el ends here
