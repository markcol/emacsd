;;; my-debug-map.el --- debug-map configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Setup of my-debug-map which other debugging modules can add keybindings
;; to.

;;; Code:

(use-package my-debug-map
  :straight (:type built-in)
  :no-require t

  :bind
  (:prefix-map my/debug-map
               :prefix "C-c -"))

(provide 'my-debug-map)
;;; my-debug-map.el ends here
