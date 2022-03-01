;;; my-explain-pause.el --- explain-pause-mode configuration. -*- lexical-binding:t ; -*-

;;; Commentary:

;; Basic configuration for explain-pause-mode.

;;; Code:

(use-package explain-pause-mode
  :straight (:type git :host github :repo "lastquestion/explain-pause-mode")
  :defer t

  :custom
  (explain-pause-blocking-too-long-ms 40))

(provide 'my-explain-pause)
;;; my-explain-pause.el ends here
