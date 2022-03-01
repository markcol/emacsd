;;; my-protobuf.el --- protobuf-mode configuration.

;;; Commentary:

;; Basic configuration for protobuf-mode.

;;; Code:

(require 'my-company)
(require 'my-flycheck)
(require 'my-folding)
(require 'my-prog-mode)

(use-package protobuf-mode
  :mode "\\.proto\\'"
  :hook
  (protobuf-mode . my/protobuf-mode-setup)

  :init
  (defun my/protobuf-mode-setup ()
    (run-hooks 'prog-mode-hook)
    (setq tab-width 2)
    (c-add-style "mhc" '((c-basic-offset . 2)
                           (indent-tabs-mode . nil)) t)

    (company-mode +1)
    (subword-mode +1)
    (my/folding))

  (defun flycheck-protobuf-buf-project-root (&optional _checker)
    "Return the nearest directory holding the buf.yaml configuration."
    (and buffer-file-name
         (locate-dominating-file buffer-file-name "buf.yaml")))

  :config
  (unbind-key "C-c C-u" 'c-mode-base-map)

  (flycheck-define-checker protobuf-buf
    "A protobuf syntax checker using buf.
See URL `https://github.com/bufbuild/buf'."
    :command ("buf" "lint" "--path" source-original)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ":" (message) line-end))
    :modes protobuf-mode
    :enabled flycheck-protobuf-buf-project-root
    :working-directory flycheck-protobuf-buf-project-root
    :predicate flycheck-buffer-saved-p)

  (add-to-list 'flycheck-checkers 'protobuf-buf))

(provide 'my-protobuf)
;;; my-protobuf.el ends here
