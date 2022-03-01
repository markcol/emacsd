;;; my-sort-symbols.el --- sort-symbols.

;;; Commentary:

;; Helper command to sort symbols in region. Shamelessly ripped from:
;; https://www.emacswiki.org/emacs/SortWords

;;; Code:

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
See `sort-symbols'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

(defalias 'ss 'sort-symbols)

(provide 'my-sort-symbols)
;;; my-sort-symbols.el ends here
