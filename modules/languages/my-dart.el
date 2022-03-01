;;; my-dart.el --- dart-mode configuration.

;;; Commentary:

;; Basic configuration for dart-mode.

;;; Code:

(require 'my-company)
(require 'my-folding)
(require 'my-lsp)
(require 'my-projectile)

(use-package dart-mode
  :mode "\\.dart\\'"
  :interpreter "dart"

  :hook
  (dart-mode . my/dart-mode-setup)

  :custom
  (dart-format-on-save t)
  (dart-enable-analysis-server t)

  :init
  (with-eval-after-load "projectile"
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

  (defun my/dart-mode-setup ()
    (when (fboundp 'highlight-symbol-mode)
      (highlight-symbol-mode -1))
    (when (fboundp 'auto-highlight-symbol-mode)
      (auto-highlight-symbol-mode -1))

    (company-mode +1)
    (lsp-deferred)
    (my/folding)
    (subword-mode +1)))

(provide 'my-dart)
;;; my-dart.el ends here
