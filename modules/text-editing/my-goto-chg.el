;;; my-goto-chg.el --- goto-chg configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for goto-chg.

;;; Code:

(use-package goto-chg
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(provide 'my-goto-chg)
;;; my-goto-chg.el ends here
