;;; my-ox-jira.el --- ox-jira configuration.

;;; Commentary:

;; Basic configuration for ox-jira.

;;; Code:

(require 'my-org-mode)

(use-package ox-jira
  :defer t

  :hook
  (org-mode . my/ox-jira-setup)

  :init
  (defun my/ox-jira-setup ()
    (require 'ox-jira)))

(provide 'my-ox-jira)
;;; my-ox-jira.el ends here
