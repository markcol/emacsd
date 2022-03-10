;;; my-env.el --- environment variable settings -*- lexical-binding: t; -*-

;;; Commentary:

;; Setup for various environment variables to ensure external programs are
;; available.

;;; Code:

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-variables '("PATH"
                                    "MANPATH"
                                    "TMPDIR"
                                    "KUBECONFIG"
                                    "GOPATH"
                                    "GOBIN"
                                    "GOROOT"
                                    "GOPRIVATE"
                                    "GOENV_GOPATH_PREFIX"
                                    "GOENV_VERSION"))
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-debug nil)

  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Set temporary-file-directory to match TMPDIR environment variable
(let ((tmpdir (getenv "TMPDIR")))
  (when (and tmpdir (not (string-blank-p tmpdir)))
    (setq temporary-file-directory tmpdir)))

(provide 'my-env)
;;; my-env.el ends here