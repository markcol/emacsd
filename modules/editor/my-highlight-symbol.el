;;; my-highlight-symbol.el --- highlight-symbol configuration. -*- lexical-binding; t; -*-

;;; Commentary:

;; Basic configuration for highlight-symbol.

;;; Code:

(use-package highlight-symbol
  :diminish highlight-symbol-mode

  :commands (highlight-symbol-prev highlight-symbol-next highlight-symbol-query-replace)
  :bind
  ("C-c C-p" . highlight-symbol-prev)
  ("C-c C-n" . highlight-symbol-next)
  ("C-c C-r" . highlight-symbol-query-replace)

  :hook
  (prog-mode . highlight-symbol-mode)

  :custom
  (highlight-symbol-highlight-single-occurrence 'nil)
  (highlight-symbol-idle-delay 0.5)
  (highlight-symbol-ignore-list '("^end$" "^def$" "^class$" "^module$")))

(provide 'my-highlight-symbol)
;;; my-highlight-symbol.el ends here
