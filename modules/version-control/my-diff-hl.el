;;; my-diff-hl.el --- diff-hl configuration.

;;; Commentary:

;; Basic configuration for diff-hl.

;;; Code:

(require 'my-magit)

(use-package diff-hl
  :hook
  (prog-mode . my/turn-on-diff-hl-mode)
  (text-mode . my/turn-on-diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  (desktop-after-read . my/diff-hl-set-render-mode)

  :custom
  (diff-hl-fringe-bmp-function 'my/diff-hl-fringe-bmp-from-type)
  (diff-hl-fringe-face-function 'my/diff-hl-fringe-face-from-type)
  (diff-hl-margin-symbols-alist
   '((insert . "┃")
     (delete . "┃")
     (change . "┃")
     (unknown . "?")
     (ignored . "i")))

  :init
  (defun my/turn-on-diff-hl-mode ()
    (turn-on-diff-hl-mode)
    (diff-hl-flydiff-mode 1))

  (defgroup my/diff-hl nil
    "Siren specific tweaks to diff-hl."
    :group 'diff-hl)

  (defface my/diff-hl-insert
    '((default :inherit diff-hl-insert))
    "Face used to highlight inserted lines."
    :group 'my/diff-hl)

  (defface my/diff-hl-delete
    '((default :inherit diff-hl-delete))
    "Face used to highlight deleted lines."
    :group 'my/diff-hl)

  (defface my/diff-hl-change
    '((default :inherit diff-hl-change))
    "Face used to highlight changed lines."
    :group 'my/diff-hl)

  (defun my/diff-hl-fringe-face-from-type (type _pos)
    (intern (format "my/diff-hl-%s" type)))

  (defun my/diff-hl-fringe-bmp-from-type(type _pos)
    (intern (format "my/diff-hl-%s" type)))

  (defun my/diff-hl-set-render-mode ()
    (diff-hl-margin-mode (if window-system -1 1)))

  :config
  (my/diff-hl-set-render-mode)

  (define-fringe-bitmap 'my/diff-hl-insert
    [#b00000011] nil nil '(center repeated))
  (define-fringe-bitmap 'my/diff-hl-change
    [#b00000011] nil nil '(center repeated))
  (define-fringe-bitmap 'my/diff-hl-delete
    [#b00000011] nil nil '(center repeated)))

(provide 'my-diff-hl)
;;; my-diff-hl.el ends here
