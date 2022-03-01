;;; my-volatile-highlights.el --- volatile-highlights-mode configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for volatile-highlights-mode.

;;; Code:

(require 'my-undo-tree)

(use-package volatile-highlights
  :demand
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree))

(provide 'my-volatile-highlights)
;;; my-volatile-highlights.el ends here
