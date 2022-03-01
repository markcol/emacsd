;;; my-move-dup.el --- move-dup

;;; Commentary:

;; Configuration for move-dup

;;; Code:

(use-package move-dup
  :bind
  ("M-p" . move-dup-move-lines-up)
  ("M-n" . move-dup-move-lines-down)
  ("C-x C-d" . move-dup-duplicate-down))

(provide 'my-move-dup)
;;; my-move-dup.el ends here
