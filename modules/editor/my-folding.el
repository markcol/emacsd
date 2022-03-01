;;; my-folding.el --- folding configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for folding code.

;;; Code:

(use-package hideshow
  :straight (:type built-in)

  :bind
  ("C-="     . my/folding-toggle-selective-display)
  ("C-c C-h" . my/folding-toggle)

  :preface
  (defun my/folding (&optional arg)
    "Activate or deactivate code folding.

Optional ARG is passed directly to mode toggle function."
    (hs-minor-mode (or arg t)))

  (defun my/folding-toggle (column)
    "Toggle hiding/showing blocks via hs-mode.

Borrowed from: http://www.emacswiki.org/emacs/HideShow"
    (interactive "P")
    (if hs-minor-mode
        (if (condition-case nil
                (hs-toggle-hiding)
              (error t))
            (hs-show-all))
      (my/folding-toggle-selective-display column)))

  (defun my/folding-toggle-selective-display (column)
    "Helper function for `my/folding-toggle'."
    (interactive "P")
    (set-selective-display
     (or column
         (unless selective-display
           (1+ (current-column)))))))

(provide 'my-folding)
;;; my-folding.el ends here
