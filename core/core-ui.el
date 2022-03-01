;;; core-ui.el - -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when window-system
  ;; use extended compound-text coding for X clipboard
  (set-selection-coding-system 'compound-text-with-extensions)
  ;; Don't minimize window
  (global-unset-key (kbd "C-z")))

(defun my/setup-frame ()
  "Configure look of FRAME."
  (when (window-system)
    (modify-all-frames-parameters '((left . 0)
                                    (top . 0)
                                    (width . 150)
                                    (fullscreen . fullheight)))
    ;; Smooth scrolling
    (setq scroll-margin 0
          scroll-conservatively 100000
          scroll-preserve-screen-position 1)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/setup-frame)
  (my/setup-frame))

(provide 'core-ui)
;;; core-ui.el ends here
