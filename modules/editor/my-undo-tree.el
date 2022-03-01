;;; my-undo-tree.el --- undo-tree configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for undo-tree.

;;; Code:

(use-package undo-tree
  ;; Use latest version of undo-tree from main git repo. The
  ;; package on elpa.gnu.org is quite old.
  :straight (:type git :host gitlab :repo "tsc25/undo-tree" :branch "master")

  :demand
  :defines (undo-tree-map)
  :commands (undo-tree-undo undo-tree-redo undo-tree-visualize)
  :bind (:map undo-tree-map
              ("C-x u" . undo-tree-visualize)
              ("M--"   . undo-tree-undo)
              ("M-_"   . undo-tree-redo)
              ("s-z"   . undo-tree-undo)
              ("s-Z"   . undo-tree-redo))

  :diminish
  undo-tree-mode

  :custom
  (undo-tree-history-directory-alist
   `((".*" . ,(expand-file-name "undo-tree-history" my/cache-dir))))
  ;; Use undohist package to persist history to disk, it seems more reliable
  ;; than undo-tree's auto-save feature which randomly fails to restore history
  ;; for no obvious reason.
  (undo-tree-auto-save-history nil)
  (undo-tree-incompatible-major-modes '(term-mode vterm-mode))

  :config
  (global-undo-tree-mode)

  ;; Unbind keys that I don't use.
  (unbind-key "C-/" undo-tree-map)
  (unbind-key "C-?" undo-tree-map)
  (unbind-key "C-_" undo-tree-map))

(provide 'my-undo-tree)
;;; my-undo-tree.el ends here
