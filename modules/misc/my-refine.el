;;; my-refine.el --- refine configuration. -*- lexical-binding: t ; -*-

;;; Commentary:

;; Basic configuration for refine.

;;; Code:

(use-package refine
  :commands refine
  :bind
  (:map refine-mode-map
        ("M-n" . refine-move-forward)
        ("M-p" . refine-move-backward)))

(provide 'my-refine)
;;; my-refine.el ends here
