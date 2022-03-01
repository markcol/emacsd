;;; my-safe-change-case.el --- change-case

;;; Commentary:

;; Set strict/safer case changing functions to C-x C-u and C-x C-l which ONLY
;; perform case modifications when region is active.

;;; Code:

(defun downcase-region-only (beg end &rest args)
  "Only downcase if region (BEG END) is active.

Avoids accidental downcase when region is not active.  Passes all
additional ARGS passed along to `downcase-region'."
  (interactive "r")
  (when (region-active-p)
    (apply #'downcase-region beg end args)))

(defun upcase-region-only (beg end &rest args)
  "Only upcase if region (BEG END) is active.

Avoids accidental upcase when region is not active.  Passes all
additional ARGS passed along to `upcase-region'."
  (interactive "r")
  (when (region-active-p)
    (apply #'upcase-region beg end args)))

(global-set-key (kbd "C-x C-l") 'downcase-region-only)
(global-set-key (kbd "C-x C-u") 'upcase-region-only)

(provide 'my-safe-change-case)
;;; my-safe-change-case.el ends here
