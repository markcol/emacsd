;;; my-marginalia.el --- marginalia configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic configuration for marginalia.

;;; Code:

(use-package marginalia
  :demand t
  :defines (marginalia-annotator-registry)
  :commands (marginalia-cycle)
  :bind (:map minibuffer-local-map

         ("M-A" . marginalia-cycle))
  :custom
  (marginalia-align-offset 10)
  (marginalia-field-width 120)
  (marginalia-separator "  ")

  :preface
  (defun marginalia-annotate-project-buffer (cand)
    "Annotate project buffer CAND with modification status, file name and major
mode."
    (when-let (buffer (get-buffer cand))
      (marginalia--fields
       ((marginalia--buffer-status buffer))
       ((marginalia--project-buffer-file buffer)
        :truncate (/ marginalia-truncate-width 2)
        :face 'marginalia-file-name))))

  (defun marginalia--project-buffer-file (buffer)
    "Return the file or process name of BUFFER relative to project root, if it
is within project root."
    (let ((root (marginalia--project-root))
          (file (marginalia--buffer-file buffer)))
      (if (string-equal root file) file
        (string-remove-prefix root file))))

  :config
  ;; Default command category to 'marginalia-annotate-binding instead of
  ;; 'marginalia-annotate-command which has a slight performance impact when
  ;; filtering M-x candidates.
  (mapc
   (lambda (x)
     (pcase (car x)
       ('command
        (setcdr x (cons 'marginalia-annotate-binding
                        (remq 'marginalia-annotate-binding (cdr x)))))))
   marginalia-annotator-registry)

  ;; Add project-buffer annotator.
  (add-to-list 'marginalia-annotator-registry
               '(project-buffer marginalia-annotate-project-buffer))

  ;; Enable marginalia.
  (marginalia-mode +1))

(provide 'my-marginalia)
;;; my-marginalia.el ends here
