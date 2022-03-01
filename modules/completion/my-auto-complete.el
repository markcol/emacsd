;;; my-auto-complete.el --- auto-complete configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for auto-complete.

;;; Code:

(require 'my-flyspell)

(use-package auto-complete
  :bind (:map ac-completing-map
              ("RET" . ac-complete)
              ("C-m" . ac-complete)
              ("C-s" . ac-isearch)
              ("C-n" . ac-next)
              ("C-p" . ac-previous))

  :custom
  (ac-auto-show-menu 0.2)
  (ac-auto-start 3)
  (ac-delay 0.05)
  (ac-menu-height 15)

  :config
  (ac-flyspell-workaround)

  ;; Auto-complete when indenting.
  (defadvice indent-for-tab-command (around ac-before-indent activate)
    "Call `auto-complete' if text was recently entered."
    (if (ac-trigger-command-p last-command)
        (auto-complete)
      ad-do-it)))

(provide 'my-auto-complete)
;;; my-auto-complete.el ends here
