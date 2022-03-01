;;; my-windmove.el --- windmove

;;; Commentary:

;; Configuration for windmove.

;;; Code:

;; Emacs 26.x and earlier
(when (version< emacs-version "27.0")
  (use-package windmove
    :straight (:type built-in)

    :bind
    ("M-i" . my/windmove-up)
    ("M-k" . my/windmove-down)
    ("M-j" . my/windmove-left)
    ("M-l" . my/windmove-right))

  (use-package buffer-move
    :bind
    ("M-K" . buf-move-down)
    ("M-I" . buf-move-up)
    ("M-J" . buf-move-left)
    ("M-L" . buf-move-right)))

;; Emacs 27.0 and later
(when (not (version< emacs-version "27.0"))
  (use-package windmove
    :straight (:type built-in)

    :bind
    ("M-i" . windmove-up)
    ("M-k" . windmove-down)
    ("M-j" . windmove-left)
    ("M-l" . windmove-right)
    ("M-K" . windmove-swap-states-down)
    ("M-I" . windmove-swap-states-up)
    ("M-J" . windmove-swap-states-left)
    ("M-L" . windmove-swap-states-right)
    ("C-x M-i" . windmove-delete-up)
    ("C-x M-k" . windmove-delete-down)
    ("C-x M-j" . windmove-delete-left)
    ("C-x M-l" . windmove-delete-right)))

;; Tmux integration with windmove
(when (and (getenv "TMUX")
           (executable-find "tmux"))
  (with-eval-after-load 'windmove
    (defun my/windmove-tmux-left-advice (orig-fun &rest args)
      "Advice windmove-left to enable Tmux integration"
      (if (not (ignore-errors (apply orig-fun args)))
          (call-process "tmux" nil nil nil "select-pane" "-L")))

    (defun my/windmove-tmux-right-advice (orig-fun &rest args)
      "Advice windmove-right to enable Tmux integration"
      (if (not (ignore-errors (apply orig-fun args)))
          (call-process "tmux" nil nil nil "select-pane" "-R")))

    (defun my/windmove-tmux-up-advice (orig-fun &rest args)
      "Advice windmove-up to enable Tmux integration"
      (if (not (ignore-errors (apply orig-fun args)))
          (call-process "tmux" nil nil nil "select-pane" "-U")))

    (defun my/windmove-tmux-down-advice (orig-fun &rest args)
      "Advice windmove-down to enable Tmux integration"
      (if (not (ignore-errors (apply orig-fun args)))
          (call-process "tmux" nil nil nil "select-pane" "-D")))

    (advice-add 'windmove-left :around 'my/windmove-tmux-left-advice)
    (advice-add 'windmove-right :around 'my/windmove-tmux-right-advice)
    (advice-add 'windmove-up :around 'my/windmove-tmux-up-advice)
    (advice-add 'windmove-down :around 'my/windmove-tmux-down-advice)))

(provide 'my-windmove)
;;; my-windmove.el ends here
