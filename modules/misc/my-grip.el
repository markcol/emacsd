;;; my-grip.el --- grip-mode configuration. -*- lexical-binding:t ; -*-

;;; Commentary:

;; Basic configuration for grip-mode.

;;; Code:

(use-package grip-mode
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

(provide 'my-grip)
;;; my-grip.el ends here
