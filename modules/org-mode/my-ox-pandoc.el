;;; my-ox-pandoc.el --- ox-pandoc configuration.

;;; Commentary:

;; Basic configuration for ox-pandoc.

;;; Code:

(require 'my-org-mode)

(use-package ox-pandoc
  :defer t

  :hook
  (org-mode . my/ox-pandoc-setup)

  :custom
  (org-pandoc-options-for-gfm '((columns . "80")))
  (org-pandoc-options-for-markdown '((columns . "80")))
  (org-pandoc-options-for-org '((columns . "80")))

  :init
  (defun my/ox-pandoc-setup ()
    (require 'ox-pandoc)))

(provide 'my-ox-pandoc)
;;; my-ox-pandoc.el ends here
