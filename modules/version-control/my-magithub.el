;;; my-magithub.el --- magithub configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for magithub.

;;; Code:

(use-package magithub
  :after magit

  :custom
  (magithub-clone-default-directory "~/src")

  :config
  (magithub-feature-autoinject t))

(provide 'my-magithub)
;;; my-magithub.el ends here
