;;; my-git-timemachine.el --- git-timemachine configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for git-timemachine.

;;; Code:

(use-package git-timemachine
  :defer t
  :init
  (defalias 'gt 'git-timemachine))

(provide 'my-git-timemachine)
;;; my-git-timemachine.el ends here
