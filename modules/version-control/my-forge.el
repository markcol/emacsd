;;; my-forge.el --- forge configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for forge.

;;; Code:

(require 'my-code-review)
(require 'my-gh-notify)
(require 'my-github-review)
(require 'my-magit)

(use-package forge
  :defer t
  :after magit

  :custom
  (forge-database-file (expand-file-name
                        "forge-database.sqlite" my/cache-dir))

  :config
  (transient-insert-suffix 'forge-dispatch '(1)
    ["Pull Request"
     ("p c" "code-review at point" code-review-forge-pr-at-point)
     ("p g" "github-review at point" github-review-forge-pr-at-point)])
  (transient-insert-suffix 'forge-dispatch '(1)
    ["GitHub"
     ("g n" "notifications" gh-notify)]))

(use-package forge-post
  :straight forge
  :defer t

  :hook
  (forge-post-mode . my/forge-post-mode-setup)

  :init
  (defun my/forge-post-mode-setup ()
    (setq-local prettier-js-args '("--parser" "markdown"
                                   "--print-width" "80"
                                   "--prose-wrap" "always"))))

(provide 'my-forge)
;;; my-forge.el ends here
