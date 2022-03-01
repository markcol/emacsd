;;; my-dired+.el --- dired+ configuration.

;;; Commentary:

;; Basic configuration for dired+.

;;; Code:

(use-package dired+
  :defer t
  :bind (:map dired-mode-map
              ("C-l" . diredp-up-directory-reuse-dir-buffer))

  :hook
  (dired-mode . my/diredp-mode-setup)

  :init
  (defun my/diredp-mode-setup ()
    (toggle-diredp-find-file-reuse-dir 1))

  :config
  (unbind-key "M-i" dired-mode-map)
  (unbind-key "M-l" dired-mode-map))

(provide 'my-dired+)
;;; my-dired+.el ends here
