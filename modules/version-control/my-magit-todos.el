;;; my-magit-todos.el --- magit-todos configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for magit-todos.

;;; Code:

(use-package magit-todos
  :after magit

  :config
  (magit-todos-mode))

(provide 'my-magit-todos)
;;; my-magit-todos.el ends here
