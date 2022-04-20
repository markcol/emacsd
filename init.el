;;; init.el --- Emacs initialization file            -*- lexical-binding: t; -*-

;;; Commentary:

;; My Emacs initialization file.

;;; Code:

(setq load-prefer-newer t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

(require 'my-funcs)
(require 'my-config)
(require 'my-package)
(require 'my-theme)
(require 'my-prog)
(require 'my-git)
(require 'my-org)

(unbind-key "s-t")
(bind-key* "C-c /" #'comment-dwim)
(when window-system
  (unbind-key "C-z"))
(bind-key "C-c a s" #'switch-to-scratch-buffer)

;; protects against accidental mouse movements
;; http://stackoverflow.com/a/3024055/1041691
(add-hook 'mouse-leave-buffer-hook
          (lambda () (when (and (>= (recursion-depth) 1)
                                (active-minibuffer-window))
                       (abort-recursive-edit))))

(use-package which-key
  :diminish
  :custom
  (which-key-enabled-define-key t)
  (which-key-show-transient-maps t)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

(use-package yasnippet
  :defer t
  :custom
  (yas-prompt-functions '(yas-completing-prompt))
  :hook
  ((
    prog-mode
    ) . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package man
  :defer t
  :custom
  (Man-notify-method 'pushy "show manpage HERE")
  :custom-face
  (Man-overstrike ((t (:inherit font-lock-type-face :bold t))))
  (Man-underline ((t (:inherit font-lock-keyword-face :underline t)))))

(use-package info-colors
  :defer t
  :commands (info-colors-fontify-mode)
  :defines (Info-selection-mode)
  :hook
  (Info-selection . info-colors-fontify-node))

(use-package helpful
  :defer t
  :commands (helpful-command helpful-at-point helpful-function helpful-callable helpful-key helpful-varaiable)
  :bind
  (
   ("C-h C"   . helpful-command)
   ("C-h C-d" . helpful-at-point)
   ("C-h F"   . helpful-function)
   ("C-h f"   . helpful-callable)
   ("C-h k"   . helpful-key)
   ("C-h v"   . helpful-variable)
   ))

(use-package elisp-demos
  :defer t
  :after helpful
  :commands (elisp-demos-advice-helpful-update elisp-demos-advice-describe-function-1)
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

(use-package unfill
  ;; fill-paragraph is nice, but emacs weirdly lacks a convenient way to
  ;; unfill paragraphs once they're filled.  This package adds that
  ;; functionality.
  :bind
  ([remap fill-paragraph] . unfill-toggle))

(use-package eldoc
  :straight nil
  :after (paredit)
  ;; :diminish eldoc-mode
  :commands eldoc-mode
  :hook
  ((elisp-mode) . eldoc-mode)
  :config
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))

(use-package restart-emacs
  ;; Restart emacs.
  :defer t)

(use-package tooltip
  :straight nil
  :demand
  :config
  (setq tooltip-resize-echo-area t)
  (tooltip-mode +1)
  )

(use-package ag
  :demand
  :ensure-system-package ag
  :init
  (setq
   ag-reuse-window t
   ag-highlight-search t)
  :hook
  (ag-search-finished-hook . (lambda ()  (pop-buffer next-error-last-buffer)))
  )

(use-package project
  :straight nil
  :defer t
  :bind (:map project-prefix-map
              ("f" . project-find-file)
              ("m" . project-compile)
              ("D" . my/project-edit-direnv)
              ("d" . project-dired)
              ("e" . my/project-edit-dir-locals)
              ("k" . my/project-kill-buffers)
              ("n" . my/project-open-new-project)
              ("p" . my/project-switch))
  :custom
  ;; This is one of my favorite things: you can customize
  ;; the options shown upon switching projects.
  (project-switch-commands
   '((project-find-file "Find file")
     ;; (magit-project-status "Magit" ?g)
     ;; (deadgrep "Grep" ?h)
     ))
  (compilation-always-kill t)
  (project-vc-merge-submodules nil)
  :preface
  (defun my/project-edit-dir-locals ()
    "Edit .dir-locals.el file in project root."
    (interactive)
    (find-file (expand-file-name ".dir-locals.el" (my/project-root))))

  (defun my/project-edit-direnv ()
    "Edit .envrc file in project root."
    (interactive)
    (find-file (expand-file-name ".envrc" (my/project-root))))

  (defun my/project-root ()
    "Return project root path."
    (project-current)
    ;; We need to extract third element because `project-current'
    ;; returns project's information as a list of 3 element,
    ;; for example (vc Git "project-path")
    (nth 2 (project-current)))

  (defun my/project-p ()
    (project-current))

  (defun my/project-name ()
    "Get project name extracting latest part of project path."
    (if (my/project-p)
        (second (reverse (split-string (my/project-root) "/")))
      nil)))

(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode))

(use-package hungry-delete
  :diminish
  :hook
  ((text-mode prog-mode) . hungry-delete-mode))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C-+" . er/contract-region)
   :map mode-specific-map
   :prefix-map region-prefix-map
   :prefix "r"
   ("(" . er/mark-inside-pairs)
   (")" . er/mark-outside-pairs)
   ("'" . er/mark-inside-quotes)
   ([34] . er/mark-outside-quotes) ; it's just a quotation mark
   ("o" . er/mark-org-parent)
   ("u" . er/mark-url)
   ("b" . er/mark-org-code-block)
   ("." . er/mark-method-call)
   (">" . er/mark-next-accessor)
   ("w" . er/mark-word)
   ("d" . er/mark-defun)
   ("e" . er/mark-email)
   ("," . er/mark-symbol)
   ("<" . er/mark-symbol-with-prefix)
   (";" . er/mark-comment)
   ("s" . er/mark-sentence)
   ("S" . er/mark-text-sentence)
   ("p" . er/mark-paragraph)
   ("P" . er/mark-text-paragraph)))

;;; init.el ends here
