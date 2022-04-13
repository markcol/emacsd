;;; init.el --- Emacs initialization file            -*- lexical-binding: t; -*-

;;; Commentary:

;; The file that starts it all.

;;; Code:

(setq load-prefer-newer t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'my-funcs)
(require 'my-config)
(require 'my-package)

(use-package paredit
  :defer t
  :hook
  ((
    cider-repl-mmode
    clojure-mode
    clojurec-mode
    clojurescript-mode
    emacs-lisp-mode
    )
   . paredit-mode))

(use-package autoinsert
  :defer t
  :hook
  (find-file . auto-insert))

(use-package checkdoc
  :defer t
  :hook
  (elisp-mode . checkdoc-minor-mode))

(use-package highlight-sexp
  :: TODO(mark): Fix highlighting color
  :disabled
  :defer t
  :hook
  ((
    clojure-mode
    emacs-lisp-mode
    lisp-mode
    )
   . highlight-sexp-mode))

;;; init.el ends here
