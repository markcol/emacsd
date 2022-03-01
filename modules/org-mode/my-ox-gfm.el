;;; my-ox-gfm.el --- ox-gfm configuration.

;;; Commentary:

;; Basic configuration for ox-gfm.

;;; Code:

(require 'my-org-mode)

(use-package ox-gfm
  :defer t

  :hook
  (org-mode . my/ox-gfm-setup)

  :init
  (defun my/ox-gfm-setup ()
    (require 'ox-gfm)))

(provide 'my-ox-gfm)
;;; my-ox-gfm.el ends here
