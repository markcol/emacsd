;;; my-zoom-window.el --- zoom-window configuration.

;;; Commentary:

;; Basic configuration for zoom-window.

;;; Code:

(require 'my-workspace-map)

(use-package zoom-window
  :bind
  (:map my/workspace-map
        ("RET" . zoom-window-zoom)
        ("C-<return>" . zoom-window-zoom))

  :config
  (with-eval-after-load "persp-mode"
    (setq zoom-window-use-persp t)
    (zoom-window-setup))

  (with-eval-after-load "elscreen"
    (setq zoom-window-use-elscreen t)
    (zoom-window-setup)))

(provide 'my-zoom-window)
;;; my-zoom-window.el ends here
