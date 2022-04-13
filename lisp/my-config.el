;;; my-config.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs configuration settings.

;;; Code:


(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(global-hl-line-mode t)
(delete-selection-mode t)
(column-number-mode)
(line-number-mode)

(with-system darwin
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))


(provide 'my-config)
;;; my-config.el ends here
