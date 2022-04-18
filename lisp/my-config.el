;;; my-config.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs configuration settings.

;;; Code:

(setq
 ;; user-full-name "Mark Colburn"
 )

(setq-default
 ;; Fill column defines wheere a paragraph should wrap.
 fill-column 80

 ;; the C-type language indentation size.
 c-basic-offet 4
 )

(setq
 ;; Don't create backup files. That's why we have version control.
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil

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
 
 ;; Support large blobs of data for LSP
 read-process-output-max (* 1024 1024)

 ;; Ensure a final newline at the end of a file
 require-final-newline t

 ;; Do not warn when loading larcge files
 large-file-warning-threshold (* 100 1024 1024)

 ;; Don't compact font caches during GC operations.
 inhibit-compacting-font-caches t

 ;; enable recursive edit in minibuffer
 enable-recursive-minibuffers t
 )

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(global-hl-line-mode +1)
(delete-selection-mode +1)
(column-number-mode +1)
(line-number-mode +1)
(minibuffer-depth-indicate-mode +1)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'describe-gnu-project 'ignore)
;; (defalias 'view-emacs-news 'ignore)

(with-system darwin
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(provide 'my-config)
;;; my-config.el ends here
