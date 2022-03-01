;;; my-uniquify.el --- uniquify configuration.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for uniquify.

;;; Code:

(use-package uniquify
  :straight (:type built-in)
  :demand t

  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-separator "/")
  ;; rename after killing uniquified
  (uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (uniquify-ignore-buffers-re "^\\*"))

(provide 'my-uniquify)
;;; my-uniquify.el ends here
