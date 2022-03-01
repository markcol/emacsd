;;; my-highlight-indent-guides.el --- highlight-indent-guides configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for highlight-indent-guides-mode.

;;; Code:

(use-package highlight-indent-guides
  :defer t
  :diminish highlight-indent-guides-mode

  :custom
  (highlight-indent-guides-auto-even-face-perc 3)
  (highlight-indent-guides-auto-odd-face-perc 2.5)
  (highlight-indent-guides-auto-top-even-face-perc 12)
  (highlight-indent-guides-auto-top-odd-face-perc 10)
  (highlight-indent-guides-character ?\u2502)
  (highlight-indent-guides-method 'column)
  (highlight-indent-guides-responsive 'top)

  :preface
  (defun my/display-indentation (&optional arg)
    "Activate or deactivate indentation guides.

Optional ARG is passed directly to mode toggle function."
    (interactive)
    (highlight-indent-guides-mode arg))
  )

(provide 'my-highlight-indent-guides)
;;; my-highlight-indent-guides.el ends here
