;;; my-savehist.el --- savehist configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for savehist.

;;; Code:

(use-package savehist
  :straight (:type built-in)

  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)
  (savehist-file (expand-file-name "savehist" my/cache-dir))

  :init
  (savehist-mode +1))

(provide 'my-savehist)
;;; my-savehist.el ends here
