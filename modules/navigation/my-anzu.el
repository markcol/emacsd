;;; my-anzu.el --- anzu configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for anzu.

;;; Code:

(use-package anzu
  :bind
  ("C-x C-r" . anzu-query-replace-regexp)
  ("M-%" . anzu-query-replace)

  :custom
  (anzu-mode-lighter "")
  (anzu-deactivate-region t)
  (anzu-search-threshold 1000)
  (anzu-replace-threshold 50)
  (anzu-replace-to-string-separator " => ")

  :config
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)

  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

  (global-anzu-mode +1))

(provide 'my-anzu)
;;; my-anzu.el ends here
