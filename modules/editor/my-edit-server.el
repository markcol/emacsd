;;; my-edit-server.el --- edit-server configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for edit-server.

;;; Code:

(use-package edit-server
  :if window-system
  :commands (edit-server-start)

  :custom
  (edit-server-default-major-mode 'markdown-mode)
  (edit-server-new-frame-alist
   '((name . "Edit with Emacs FRAME")
     (width . 120)
     (height . 60)
     (minibuffer . t)
     (menu-bar-lines . t)))

  :config
  (edit-server-start))

(provide 'my-edit-server)
;;; my-edit-server.el ends here
