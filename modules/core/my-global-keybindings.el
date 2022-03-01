;;; my-global-keybindings.el --- Global keybindings. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic global keybindings.

;;; Code:

;; Enable alternative to M-x.
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Enable dabbrev-expand via custom keybinding.
(global-set-key (kbd "C-x M-/") 'dabbrev-expand)

;; Easier version of "C-x k" to kill buffer
(global-set-key (kbd "C-x C-k") 'kill-buffer)

;; Evaluate buffer
(global-set-key (kbd "C-c C-e") 'eval-buffer)

;; Window switching
(global-set-key (kbd "C-x i")   'my/other-window-reverse)
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-x C-i") 'my/other-window-reverse)

;; Window management
(global-set-key (kbd "C-x C-SPC") 'balance-windows)
(global-set-key (kbd "C-x SPC") 'balance-windows)

;; Kill-Ring related
(global-set-key (kbd "M-Y") 'my/yank-pop-forwards)

;; Align to equal signs
(global-set-key (kbd "C-x a =") 'my/align-region-to-equals)
(global-set-key (kbd "C-x a {") 'my/align-region-to-opening-brace)

;; align-regexp
(global-set-key (kbd "C-c a") 'align-regexp)

;; Toggle auto-fill-mode.
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; iBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Rename current file and buffer
(global-set-key (kbd "C-c r")  'my/rename-file-and-buffer)

;; Mac OS X specific keybindings
(when (eq system-type 'darwin)
  ;; Move to beginning/end of buffer
  (global-set-key (kbd "s-<up>") 'beginning-of-buffer)
  (global-set-key (kbd "s-<down>") 'end-of-buffer)

  ;; Move to beginning/end of line
  (global-set-key (kbd "s-<left>") 'beginning-of-line)
  (global-set-key (kbd "s-<right>") 'end-of-line))

(provide 'my-global-keybindings)
;;; my-global-keybindings.el ends here
