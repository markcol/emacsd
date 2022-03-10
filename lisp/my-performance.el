;;; my-performance.el --- Performance tweaks -*- lexical-binding: t; -*-

;;; Commentary:

;; Performance tweaks.

;;; Code:

;; Setup and use gcmh-mode for improved garbage collection.
(use-package gcmh
  :demand
  :commands (gcmh-idle-garbage-collect)
  :hook
  (focus-out-hook . gcmh-idle-garbage-collect)

  :custom
  (gcmh-idle-delay 10)
  ;; (gcmh-high-cons-threshold 104857600)

  :config
  (gcmh-mode +1))

(provide 'my-performance)
;;; my-performance.el ends here
