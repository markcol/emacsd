;;; my-prog.el --- programming language configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Programming language configuration.

;;; Code:

(require 'use-package)

(use-package eldoc
  ;; eldoc shows useful information in the minibuffer and is enabled by
  ;; default.
  :straight nil
  :defer 1
  :config
  ;; No need to delay showing eldoc
  (setq eldoc-idle-delay 0))

(use-package lisp-mode
  :straight nil
  :defer t
  :commands (emacs-lisp-mode)
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-b" . eval-buffer)
	("RET" . comment-indent-new-line))
  :hook
  (emacs-lisp-mode . my/emacs-lisp-mode-hook)
  :preface
  (defun my/emacs-lisp-mode-hook ()
    "Setup stuff for elisp."
    ;; Sentences end with a double space in elisp:
    (setq-local sentence-end-double-space t)))

(use-package paredit
  :defer t
  :diminish "()"
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
  :commands checkdoc-minor-mode
  :hook
  (elisp-mode . checkdoc-minor-mode))

(use-package aggressive-indent
  ;; Keep code indented automatically
  :defer 10
  :init
  ;; The whole point of this mode is so I don't have to indent things
  ;; manually; why in the world is it setting up a keymap!?
  (setq aggressive-indent-mode-map (make-sparse-keymap))
  :config
  :hook
  (global-aggressive-indent-mode))

(use-package autorevert
  :defer 1
  :hook
  (dired-mode . auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  :config
  ;; Emacs should refresh buffers automatically so if they've changed on
  ;; disk the buffer will update.
  (global-auto-revert-mode))

(use-package comint
  :straight nil
  :demand
  ;; comint is the mode from which inferior processes inherit, like the
  ;; python REPL or iESS modes (the R console)
  :config
  (setq comint-prompt-read-only t)
  (setq comint-move-point-for-output nil)
  (setq comint-scroll-to-bottom-on-input 'this))

(use-package compile
  :straight nil
  :demand
  :bind
  ("<f8>" . compile)
  :hook
  ((shell-mode . compilation-shell-minor-mode)
   ;; Wrap lines in compilation buffers
   (compilation-mode . visual-line-mode))
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error)
  (compile-command "make -k -j4 "))

(use-package diff-mode
  :straight nil
  :config
  (setq diff-font-lock-prettify t))

(use-package prog-mode
  :straight nil
  :defer t
  )

(use-package project
  :straight nil
  :defer t
  :init
  (setq project-vc-merge-submodules nil))

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

(use-package highlight-defined
  :ensure t
  :custom
  (highlight-defined-face-use-itself t)
  :hook
  (help-mode . highlight-defined-mode)
  (emacs-lisp-mode . highlight-defined-mode))

(use-package tree-sitter
  :hook
  ((
    bash-mode
    c-mode
    go-mode
    js-mode
    python-mode
    ruby-mode
    rust-mode
    sh-mode
    typescript-mode
    )
    . tree-sitter-hl-mode)
  :config
  (use-package tree-sitter-langs))

(use-package ssh-config-mode
  :defer t
  :mode
  (("/\\.ssh/config\\'"     . ssh-config-mode)
   ("/sshd?_config\\'"      . ssh-config-mode)
   ("/known_hosts\\'"       . ssh-known-hosts-mode)
   ("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
  :hook
  (ssh-config-mode . turn-on-font-lock)
  :init
  (autoload 'ssh-config-mode "ssh-config-mode" t))

(use-package markdown-mode
  :defer t
  :ensure-system-package markdown
  :commands (markdown-previous-visible-heading markdown-next-visible-heading)
  :defines (markdown-enable-math markdown-fontify-code-blocks-natively)
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.Rmd\\'"         . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))
  :bind
  (:map markdown-mode-map
        ("M-p" . markdown-previous-visible-heading)
        ("M-n" . markdown-next-visible-heading))
  :custom
  (markdown-command "markdown")
  :config
  (setq markdown-enable-math t
        markdown-fontify-code-blocks-natively t)
  )

(use-package json-mode
  :defer t)

(use-package csv-mode
  :defer t
  :mode
  (("\\.[Cc][Ss][Vv]\\'" . csv-mode)))

(use-package sh-script
  :defer t
  :mode (("zshecl" . sh-mode)
         ("\\.zsh\\'" . sh-mode))
  :custom
  ;; zsh
  (system-uses-terminfo nil))

(use-package executable
  :commands executable-make-buffer-file-executable-if-script-p
  :hook
  ;; Make scripts executable so I don't have to remember to do so.
  (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package jira-markup-mode
  :disabled
  :defer t
  :after atomic-chrome
  :mode ("\\.confluence$" . jira-markup-mode)
  :custom-update
  (atomic-chrome-url-major-mode-alist
   '(("atlassian\\.net$" . jira-markup-mode))))

(provide 'my-prog)
;;; my-prog.el ends here
