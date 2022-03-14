;;; my-lisp.el --- lisp/elisp settings -*- lexical-binding:t; -*-

;;; Commentary:

;; Lisp and emacs-lisp related configuration.

;;; Code:

(use-package paredit
  :diminish "()"
  :commands (paredit-mode enable-paredit-mode paredit-forward-slurp-sexp
                          paredit-forward-barf-sexp paredit-close-parenthesis)
  :bind (("M-)" . paredit-forward-slurp-sexp)
         ("C-(" . paredit-forward-barf-sexp)
         (")"   . paredit-close-parenthesis))
  :hook ((cider-repl-mode
          clojure-mode
          emacs-lisp-mode
          geiser-repl-mode
          ielm-mode
          lisp-interaction-mode
          lisp-mode
          scheme-mode
          slime-repl-mode) . enable-paredit-mode))

(provide 'my-lisp)
;;; my-lisp.el ends here
