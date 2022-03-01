;;; my-dash-at-point.el --- dash-at-point configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for dash-at-point.

;;; Code:

(use-package dash-at-point
  :bind
  ("C-c d" . dash-at-point)
  ("C-c e" . dash-at-point-with-docset))

(provide 'my-dash-at-point)
;;; my-dash-at-point.el ends here
