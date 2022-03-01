;;; my-ivy.el --- ivy configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for ivy.

;;; Code:

(use-package ivy
  :defer t
  :bind
  ("C-c C-r" . ivy-resume)

  :custom
  (ivy-use-virtual-buffers nil)
  (ivy-count-format "%d ")
  (ivy-re-builders-alist '((amx-completing-read-ivy . ivy--regex-fuzzy)
                           (t . ivy--regex-plus))))

(use-package ivy-prescient
  :after (ivy prescient)

  :config
  (ivy-prescient-mode +1))

(provide 'my-ivy)
;;; my-ivy.el ends here
