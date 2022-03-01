;;; my-flyspell.el --- flyspell configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for flyspell.

;;; Code:

(use-package flyspell
  :straight (:type built-in)
  :defer t
  :diminish flyspell-mode
  :commands (flyspell-prog-mode)

  :hook
  (prog-mode . flyspell-prog-mode)

  :custom
  (ispell-program-name "aspell") ;; use aspell instead of ispell
  (ispell-extra-args '("--lang=en" "--sug-mode=ultra"))

  :config
  (use-package flyspell-correct
    :commands (flyspell-correct-wrapper)
    :bind ("C-/" . flyspell-correct-wrapper)))

(provide 'my-flyspell)
;;; my-flyspell.el ends here
