;;; my-config.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs configuration settings.

;;; Code:

(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil
 )

(setq
 ;; Double-spaces after periods is morally wrong.
 sentence-end-double-space nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; Save existing clipboard text into the kill ring before replacing it.
 save-interprogram-paste-before-kill t
 ;; Prompts should go in the minibuffer, not in a GUI.
 use-dialog-box nil
 ;; Fix undo in commands affecting the mark.
 mark-even-if-inactive nil
 ;; Let C-k delete the whole line.
 kill-whole-line t
 ;; search should be case-sensitive by default
 case-fold-search nil
 ;; no need to prompt for the read command _every_ time
 compilation-read-command nil
 ;; scroll to first error
 compilation-scroll-output 'first-error
 ;; my source directory
 default-directory "~/src/"
 ;; eke out a little more scrolling performance
 fast-but-imprecise-scrolling t
 ;; prefer newer elisp files
 load-prefer-newer t
 ;; when I say to quit, I mean quit
 confirm-kill-processes nil
 ;; if native-comp is having trouble, there's not very much I can do
 ;; native-comp-async-report-warnings-errors 'silent
 ;; unicode ellipses are better
 truncate-string-ellipsis "…"
 )

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(global-hl-line-mode t)
(delete-selection-mode t)
(column-number-mode)
(line-number-mode)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(with-system darwin
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(provide 'my-config)
;;; my-config.el ends here
