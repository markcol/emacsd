;;; my-shift-text.el --- shift-text

;;; Commentary:

;; Configuration for shift-text

;;; Code:

(defun my/shift-right (&optional arg)
  "Shift the line or region to the ARG places to the right.
A place is considered `tab-width' character columns."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) tab-width))))

(defun my/shift-left (&optional arg)
  "Shift the line or region to the ARG places to the left."
  (interactive)
  (my/shift-right (* -1 (or arg 1))))

(global-set-key (kbd "C-c [") 'my/shift-left)
(global-set-key (kbd "C-c ]") 'my/shift-right)
(global-set-key (kbd "M-{") 'my/shift-left)
(global-set-key (kbd "M-}") 'my/shift-right)

(provide 'my-shift-text)
;;; my-shift-text.el ends here
