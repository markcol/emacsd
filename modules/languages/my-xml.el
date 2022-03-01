;;; my-xml.el --- XML editing configuration.

;;; Commentary:

;; Basic configuration for XML editing.

;;; Code:

(require 'my-prettier-js)
(require 'my-prog-mode)

(use-package nxml-mode
  :straight (:type built-in)
  :defer t
  :hook (nxml-mode . my/xml-setup)

  :custom
  (nxml-attribute-indent 2)
  (nxml-child-indent 2)

  :init
  (defun my/xml-setup ()
    (run-hooks 'prog-mode-hook)
    (setq tab-width 2)
    (prettier-js-mode)))

(provide 'my-xml)
;;; my-xml.el ends here
