;;; my-recentf.el --- recentf configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for recentf.

;;; Code:

(use-package recentf
  :straight (:type built-in)
  :demand t

  :custom
  (recentf-save-file (expand-file-name "recentf" my/cache-dir))
  (recentf-max-saved-items 5000)
  (recentf-max-menu-items 1000)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '("\\.git.*" "\\.hg.*" "\\.svn.*"))

  :init
  (defun my/recentf-exclude-p (file)
    "A predicate to decide whether to exclude FILE from recentf."
    (let ((file-dir (file-truename (file-name-directory file))))
      (-any-p (lambda (dir)
                (string-prefix-p dir file-dir))
              (mapcar 'file-truename (list my/cache-dir package-user-dir)))))

  :config
  (add-to-list 'recentf-exclude 'my/recentf-exclude-p)
  (recentf-mode +1))

(provide 'my-recentf)
;;; my-recentf.el ends here
