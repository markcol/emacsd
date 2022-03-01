;;; my-prog-mode.el --- programming language configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration shared across all programming languages.

;;; Code:

(use-package prog-mode
  :straight (:type built-in)
  :hook
  (prog-mode . my/prog-mode-setup)

  :preface
  (defun my/prog-mode-setup ()
    "Default coding hook, useful with any programming language."
    (setq-local fill-column 80)

    ;; Only show indentation if file size is below 100KB. It tends to cause a lot
    ;; of lag and slowdowns on larger files, especially YAML files.
    (if (< (buffer-size) (* 100 1024))
        (my/display-indentation 1))

    (hl-line-mode t)
    (visual-line-mode t)
    (whitespace-mode t))

  :init
  (add-to-list 'safe-local-variable-values
               '(fill-column . 120)))

(provide 'my-prog-mode)
;;; my-prog-mode.el ends here
