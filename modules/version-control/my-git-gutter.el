;;; my-git-gutter.el --- git-gutter configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for git-gutter.

;;; Code:

(use-package git-gutter
    :hook
    (prog-mode . git-gutter-mode)
    (text-mode . git-gutter-mode)

    :custom
    (git-gutter:update-interval 1)
    (git-gutter:added-sign "+")
    (git-gutter:modified-sign "=")
    (git-gutter:deleted-sign "-"))

(use-package git-gutter-fringe
  :if window-system
  :after git-gutter
  :config
  ;; Customize fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added
    [#b00000011] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified
    [#b00000011] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [#b00000011] nil nil '(center repeated)))

(provide 'my-git-gutter)
;;; my-git-gutter.el ends here
