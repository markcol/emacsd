;;; my-sql.el --- sql-mode configuration.

;;; Commentary:

;; Basic configuration for sql-mode.

;;; Code:

(require 'my-rainbow)

(use-package sql-mode
  :straight (:type built-in)
  :mode "\\.sql\\'"
  :hook (sql-mode . my/sql-mode-setup)

  :init
  (defun my/sql-mode-setup ()
    (setq tab-width 2)

    (company-mode +1)))

(use-package lsp-sqls
  :straight lsp-mode

  :hook
  (sql-mode . lsp-deferred)

  :custom
  (lsp-sqls-connections
   '(((driver . "mysql") (dataSourceName . "root@tcp(localhost:3306)/")))))

(use-package sqlformat
  :hook
  (sql-mode . sqlformat-on-save-mode)

  :custom
  (sqlformat-args '("-g"))
  (sqlformat-command 'pgformatter))

(provide 'my-sql)
;;; my-sql.el ends here
