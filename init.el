;;; init.el -*- lexical-binding: t; -*-

;;; Commentary:

;; The file that starts it all.

;;; Code:

(setq load-prefer-newer t)
(setq comp-deferred-compilation t)

(load (expand-file-name "core/core-init.el"
                        (file-name-directory load-file-name)))

;;; init.el ends here
