;;; my-modules.el --- module settings -*- lexical-binding:t; -*-

;;; Commentary:

;; Install and configure various packages.

;;; Code:

(require 'my-lisp)
(require 'my-completion)

(use-package restart-emacs
  :commands restart-emacs)

(use-package centaur-tabs
  :demand
  :requires all-the-icons
  :commands (centuar-tabs-local-mode)
  :hook ((dired-mode dashboard-mode term-mode calendar-mode org-agenda-mode helpful-mode) . centaur-tabs-local-mode)
  :preface
  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))

       ;; Buffer name not match below blacklist.
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*mybuf" name)

       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
	    (not (file-name-extension name)))
       )))
  
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

 Group centaur-tabs with mode if buffer is derived from `eshell-mode'
 `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
	   (memq major-mode '(magit-process-mode
			      magit-status-mode
			      magit-diff-mode
			      magit-log-mode
			      magit-file-mode
			      magit-blob-mode
			      magit-blame-mode
			      )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
			  help-mode))
       "Help")
      ((memq major-mode '(org-mode
			  org-agenda-clockreport-mode
			  org-src-mode
			  org-agenda-mode
			  org-beamer-mode
			  org-indent-mode
			  org-bullets-mode
			  org-cdlatex-mode
			  org-agenda-log-mode
			  diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  
  :config
  (setq centuar-tabs-style "rounded"
        centuar-tabs-height 32
        centuar-tabs-set-icons t
        centuar-tabs-set-modified-marker t
        centuar-tabs-set-bar 'bar)
  ;; (setq uniquify-separator "/")         
  ;; (setq uniquify-buffer-name-style 'forward)
  ;; (centaur-tabs-headline-match)
  ;; (setq centaur-tabs-gray-out-icons 'buffer)
  ;; (centaur-tabs-enable-buffer-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  
  :bind
  ("C-s-<left>"   . centaur-tabs-backward)
  ("C-s-<right>"  . centaur-tabs-forward)
  ;; ("C-c t s" . centaur-tabs-counsel-switch-group)
  ;; ("C-c t p" . centaur-tabs-group-by-projectile-project)
  ;; ("C-c t g" . centaur-tabs-group-buffer-groups)
  )

(provide 'my-modules)
;;; my-modules.el ends here
