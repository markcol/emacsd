;;; init.el -*- lexical-binding: t; -*-

;;; Commentary:

;; The file that starts it all.

;;; Code:

(setq load-prefer-newer t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'my-funcs)
(require 'my-config)
(require 'my-package)


(use-package paredit
  :hook ((
	  emacs-lisp-mode
	  clojure-mode
	  clojurescript-mode
	  clojurec-mode
	  cider-repl-mmode
	  )
	 . paredit-mode))

;;; init.el ends here
