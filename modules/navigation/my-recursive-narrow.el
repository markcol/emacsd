;;; my-recursive-narrow.el --- recursive-narrow configuration.

;;; Commentary:

;; Basic configuration for recursive-narrow.

;;; Code:

(use-package recursive-narrow
  :bind
  ("C-x C-n" . recursive-narrow-or-widen-dwim)
  ("C-x n w" . recursive-widen)
  ("C-x n n" . recursive-narrow-or-widen-dwim)

  :config
  (add-hook 'recursive-narrow-dwim-functions
            'my/recursive-narrow-org-edit-src-code)
  (add-hook 'recursive-narrow-dwim-functions
            'my/recursive-narrow-markdown-edit-code-block)

  :init
  (defun my/recursive-narrow-org-edit-src-code()
    (ignore-errors (org-edit-src-code) t))

  (defun my/recursive-narrow-markdown-edit-code-block()
    (ignore-errors (markdown-edit-code-block) t)))

(provide 'my-recursive-narrow)
;;; my-recursive-narrow.el ends here
