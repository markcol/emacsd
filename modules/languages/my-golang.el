;;; my-golang.el --- Go configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for go-mode.

;;; Code:

(require 'my-company)
(require 'my-dap)
(require 'my-flycheck)
(require 'my-folding)
(require 'my-lsp)
(require 'my-projectile)
(require 'my-reformatter)

(use-package go-mode
  :mode "\\.go\\'"
  :interpreter "go"
  :commands go-mode
  :bind (:map go-mode-map
              ("RET" . newline-and-indent)
              ("C-h f" . godoc-at-point))

  :hook
  (go-mode . my/go-mode-setup)

  :init
  (with-eval-after-load "projectile"
    (add-to-list 'projectile-globally-ignored-directories "Godeps")
    (add-to-list 'projectile-globally-ignored-directories "vendor/github.com")
    (add-to-list 'projectile-globally-ignored-directories "vendor/gopkg.in"))

  (defun my/go-mode-setup ()
    (setq-local tab-width 4
                company-minimum-prefix-length 1
                whitespace-style (delete 'indentation whitespace-style))

    (when (fboundp 'highlight-symbol-mode)
      (highlight-symbol-mode -1))
    (when (fboundp 'auto-highlight-symbol-mode)
      (auto-highlight-symbol-mode -1))

    (my/display-indentation -1)
    (company-mode +1)
    (my/folding)
    (subword-mode +1))

  (defun my/define-golines-format-mode ()
    ;; Setup golines formatter for manual use - on save formatting is handled by
    ;; lsp-mode. See https://github.com/segmentio/golines.
    (reformatter-define golines-format
      :program "golines"
      :args '("-t" "4" "-m" "80" "--no-reformat-tags")
      :lighter "GOLINES"))

  :config
  ;; (my/define-golines-format-mode)

  (define-key 'help-command (kbd "G") 'godoc)

  ;; Ignore go test -c output files
  (add-to-list 'completion-ignored-extensions ".test"))

(use-package lsp-go
  :straight lsp-mode

  :hook
  (go-mode . my/lsp-go-mode-setup)

  :custom
  (lsp-go-use-placeholders t)
  (lsp-go-link-target "pkg.go.dev")
  (lsp-go-use-gofumpt t)
  (lsp-go-analyses '((nilness . t)
                     (shadow . t)
                     (unusedparams . t)
                     (unusedwrite . t)))

  :preface
  (defun my/lsp-go-mode-setup ()
    (setq-local my/lsp-format-buffer-func 'my/lsp-go-format-buffer)
    (lsp-format-buffer-on-save-mode t)
    (lsp-organize-imports-on-save-mode t)
    (lsp-deferred))

  (defun my/lsp-go-format-buffer ()
    (lsp-format-buffer)
    ;; (golines-format-buffer)
    )

  :config
  ;; Create custom lsp-client for golangci-lint-langserver.
  (lsp-register-custom-settings
   '(("golangci-lint.command" ["golangci-lint" "run" "--out-format" "json"])))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     '("golangci-lint-langserver"))
                    :major-modes '(go-mode)
                    :language-id "go"
                    :priority 0
                    :server-id 'golangci-lint
                    :add-on? t
                    :library-folders-fn #'lsp-go--library-default-directories
                    :initialization-options (lambda ()
                                              (gethash "golangci-lint"
                                                       (lsp-configuration-section "golangci-lint")))))
  (add-to-list 'lsp-language-id-configuration '(go-mode . "golangci-lint"))
  )

(use-package go-dlv
  :defer t
  :after (go-mode))

(use-package gotest
  :defer t
  :after (go-mode)
  :bind (:map go-mode-map
              ("C-c , a" . go-test-current-project)
              ("C-c , v" . go-test-current-file)
              ("C-c , s" . go-test-current-test)
              ("C-c , c" . go-test-current-coverage)
              ("C-c , b" . go-test-current-benchmark)
              ("C-c , B" . go-test-current-project-benchmarks)
              ("C-c , r" . go-run))

  :custom
  (go-test-verbose t))

(use-package dap-go
  :straight dap-mode
  :after (go-mode)
  :bind (:map dap-mode-map
              ("C-c , d" . my/dap-go-debug-current-test))

  :preface
  (defun my/dap-go-debug-current-test ()
    (interactive)
    (let ((name (go-test--get-current-test)))
      (dap-debug
       (list :type "go"
             :request "launch"
             :name (concat "Go: Debug " name " test")
             :mode "auto"
             :program "${fileDirname}"
             :buildFlags nil
             :args (concat "-test.run ^" name "$")
             :env nil
             :envFile nil)))))

(use-package go-gen-test
  :defer t
  :after (go-mode)
  :bind (:map go-mode-map
              ("C-c , g" . go-gen-test-dwim)
              ("C-c , G" . go-gen-test-exported)))

(use-package go-projectile
  :defer t
  :after (go-mode)
  :hook (go-mode . my/go-projectile-setup)

  :custom
  ;; prevent go-projectile from screwing up GOPATH
  (go-projectile-switch-gopath 'never)

  :preface
  (defun my/go-projectile-setup ()))

(use-package go-playground
  :defer t
  :after (go-mode))

(provide 'my-golang)
;;; my-golang.el ends here
