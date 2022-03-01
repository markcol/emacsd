;;; core-custom.el --- customizable variables. -*- lexical-binding: t; -*-

;;; Commentary:

;; Refinements of the core editing experience in Emacs.
;;
;; Shamelessly ripped from Emacs Prelude.

;;; Code:

(defvar my/yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defvar my/indent-sensitive-modes
  '(conf-mode haml-mode makefile-automake-mode makefile-bsdmake-mode
                makefile-gmake-mode makefile-imake-mode makefile-makepp-mode
                makefile-mode python-mode yaml-mode)
  "Major modes for which auto-indenting is suppressed.")

(defvar my/yank-indent-modes '(LaTeX-mode TeX-mode)
  "Major modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

(defvar my/transparency-level 99
  "The default frame transparency level for Emacs frames.")

(provide 'core-custom)
;;; core-custom.el ends here
