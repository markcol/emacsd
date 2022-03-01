;;; my-markdown.el - -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for markdown-mode.

;;; Code:

(require 'my-display-fill-column)
(require 'my-display-line-numbers)
(require 'my-flyspell)
(require 'my-move-dup)
(require 'my-prettier-js)
(require 'my-smartparens)

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md" . markdown-mode)
   ("\\.mkd" . markdown-mode)
   ("\\.mkdn" . markdown-mode)
   ("\\.mdown" . markdown-mode)
   ("\\.markdown" . markdown-mode))

  :bind
  (:map markdown-mode-map
        ("C-c p" . markdown-preview)
        ("M-p" . md-move-lines-up)
        ("M-n" . md-move-lines-down)
        ("M-P" . markdown-previous-link)
        ("M-N" . markdown-next-link))

  :hook
  (markdown-mode . my/markdown-mode-setup)

  :custom
  (markdown-command "pandoc -f gfm -t html5")

  :custom-face
  (markdown-code-face ((t nil)))

  :init
  (defun my/markdown-mode-setup ()
    ;; Configure prettier after local vars are processed, allowing local
    ;; override of fill-column and have prettier respect it.
    (add-hook 'hack-local-variables-hook
              'my/markdown-mode-setup-prettier nil t)

    (setq-local markdown-asymmetric-header t
                whitespace-action nil)

    (my/display-fill-column)
    (my/display-line-numbers)
    (auto-fill-mode)
    (prettier-js-mode)
    (flyspell-mode)
    (smartparens-mode +1)
    (subword-mode))

  (defun my/markdown-mode-setup-prettier ()
    (setq-local prettier-js-args `("--parser" "markdown"
                                   "--print-width" ,(number-to-string fill-column)
                                   "--prose-wrap" "always"))))

;; Required by markdown-edit-code-block.
(use-package edit-indirect
  :defer t)

(provide 'my-markdown)
;;; my-markdown.el ends here
