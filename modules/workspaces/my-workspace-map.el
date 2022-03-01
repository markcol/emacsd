;;; my-workspace-map.el --- workspaces-map setup.

;;; Commentary:

;; Setup of my/workspace-map which other workspace modules can add
;; keybindings to.

;;; Code:

(use-package my/workspace-map
  :straight (:type built-in)
  :no-require t

  :bind
  (:prefix-map my/workspace-map
               :prefix "C-z"))

(provide 'my-workspace-map)
;;; my-workspace-map.el ends here
