;;; core-modules.el - -*- lexical-binding: t; -*-

;;; Commentary:

;; Load the modules!

;;; Code:

(defvar my/modules-dir (expand-file-name "modules" user-emacs-directory)
  "Root directory for Emacs modules.")

(my/recursive-add-to-load-path my/modules-dir)

;; Theme
(require 'my-doom-themes)

;; Core
(require 'my-aliases)
(require 'my-global-keybindings)
(require 'my-packages)

;; Completion
(require 'my-company)

;; Documentation
(require 'my-dash-at-point)
(require 'my-helpful)

;; Editor
(require 'my-edit-server)
(require 'my-display-fill-column)
(require 'my-highlight-indentation)
(require 'my-display-line-numbers)
(require 'my-embark)
(require 'my-folding)
(require 'my-highlight-symbol)
(require 'my-marginalia)
(require 'my-minions)
(require 'my-mwim)
(require 'my-rainbow)
(require 'my-recentf)
(require 'my-savehist)
(require 'my-undo-tree)
(require 'my-undohist)
(require 'my-uniquify)
(require 'my-which-key)

;; Completion Systems and Interfaces
;;; (require 'my-vertico)
;; (require 'my-orderlies)

;; Linting
(require 'my-flycheck)

;; Misc.
(require 'my-explain-pause)
(require 'my-grip)
(require 'my-lorem-ipsum)
(require 'my-rand)
(require 'my-restart-emacs)
(require 'my-refine)
(require 'my-uuidgen)
(require 'my-zone)

;; Navigation
(require 'my-anzu)
(require 'my-avy)
(require 'my-centaur-tabs)
(require 'my-consult)
(require 'my-ctrlf)
(require 'my-dired)
(require 'my-direx)
(require 'my-dumb-jump)
(require 'my-git-link)
(require 'my-helm)
(require 'my-helm-ag)
(require 'my-helm-swoop)
(require 'my-imenu)
(require 'my-recursive-narrow)
(require 'my-scroll-half-screen)

;; Project management
(require 'my-editorconfig)
(require 'my-projectile)
(require 'my-treemacs)

;; Language Servers
(require 'my-lsp)
(require 'my-consult-lsp)
(require 'my-helm-lsp)

;; Debugging
(require 'my-dap)

;; Shell
(require 'my-shell-pop)
(require 'my-vterm)
(require 'my-xterm-color)

;; Spelling
(require 'my-flyspell)

;; Text editing
(require 'my-expand-region)
(require 'my-move-dup)
(require 'my-multiple-cursors)
(require 'my-randomize-region)
(require 'my-safe-change-case)
(require 'my-smart-shift)
(require 'my-smartparens)
(require 'my-sort-symbols)
(require 'my-sort-words)
(require 'my-string-edit)
(require 'my-string-inflection)
(require 'my-toggle-comments)
(require 'my-toggle-quotes)
 (require 'my-yasnippet)

;; Formatting
(require 'my-whitespace-cleanup)

;; Version control
(require 'my-code-review)
(require 'my-diff-hl)
(require 'my-ediff)
(require 'my-forge)
(require 'my-gh-notify)
(require 'my-git-timemachine)
(require 'my-github)
(require 'my-github-review)
(require 'my-magit)

;; Window management
(require 'my-resize-window)
(require 'my-windmove)
(require 'my-zoom-window)

;; Workspace management
(require 'my-tab-bar)
(require 'my-desktop)

;; Org-mode
(require 'my-htmlize)
(require 'my-ob-go)
(require 'my-ob-http)
(require 'my-ob-mermaid)
(require 'my-org-mode)
(require 'my-ox-gfm)
(require 'my-ox-pandoc)

;; Tools
(require 'my-docker)
(require 'my-helm-make)
(require 'my-httprepl)
(require 'my-kubernetes)
(require 'my-makefile-executor)
(require 'my-restclient)

;; XWidgets
(require 'my-xwwp)

;; Major modes
(require 'my-fundamental-mode)
(require 'my-prog-mode)
(require 'my-special-mode)
(require 'my-text-mode)

;; Languages
(require 'my-conf)
(require 'my-css)
(require 'my-dart)
(require 'my-dockerfile)
(require 'my-emacs-lisp)
(require 'my-flutter)
(require 'my-git-modes)
(require 'my-golang)
(require 'my-groovy)
(require 'my-haml)
(require 'my-haskell)
(require 'my-jinja2)
(require 'my-js)
(require 'my-json)
(require 'my-jsonnet)
(require 'my-jsx)
(require 'my-lisp)
(require 'my-makefile)
(require 'my-markdown)
(require 'my-mermaid)
(require 'my-nginx)
(require 'my-php)
(require 'my-plantuml)
(require 'my-protobuf)
(require 'my-rust)
(require 'my-sass)
(require 'my-scss)
(require 'my-sh)
(require 'my-sql)
(require 'my-ssh-config)
(require 'my-terraform)
(require 'my-toml)
(require 'my-typescript)
(require 'my-web-mode)
(require 'my-xml)
(require 'my-yaml)

(provide 'core-modules)
;;; core-modules.el ends here
