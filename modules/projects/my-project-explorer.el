;;; my-project-explorer.el - -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for project-explorer.

;;; Code:

(use-package project-explorer
  :bind
  ("C-x C-p" . project-explorer-toggle)

  :custom
  (pe/follow-current t)
  (pe/width 54)
  (pe/cache-directory (expand-file-name "project-explorer" my/cache-dir))

  :config
  ;; Make Project Explorer open selected file in last used buffer
  (setq pe/display-content-buffer-function
        (lambda (buffer)
          (let* (( last-buffer
                   (car (cl-remove 'project-explorer-mode
                                   (buffer-list)
                                   :key (apply-partially 'buffer-local-value
                                                         'major-mode))))
                 ( window (get-buffer-window last-buffer)))
            (if window
                (set-window-buffer window buffer)
              (pe/show-buffer buffer)))
          )))

(provide 'my-project-explorer)
;;; my-project-explorer.el ends here
