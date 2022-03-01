;;; my-php.el --- php-mode configuration.

;;; Commentary:

;; Basic configuration for php-mode.

;;; Code:

(require 'my-company)
(require 'my-folding)
(require 'my-prettier-js)
(require 'my-rainbow)

(use-package php-mode
  :interpreter "php"
  :mode "\\.php\\'" "\\.inc\\'" "\\.module\\'"
  :hook
  (php-mode . my/php-mode-setup)

  :init
  (defun my/php-mode-setup ()
    (prettier-js-mode)
    (rainbow-mode +1)
    (company-mode +1)
    (subword-mode +1)
    (my/folding)))

(provide 'my-php)
;;; my-php.el ends here
