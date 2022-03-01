;;; my-swiper.el --- swiper configuration.

;;; Commentary:

;; Basic configuration for swiper.

;;; Code:

(require 'my-ivy)

(use-package swiper
  :bind
  ("C-s" . swiper)

  :custom-face
  (ivy-current-match ((t (:background "#7e9fc9" :foreground "black"))))
  (swiper-line-face ((t (:background "#313c4d"))))

  :custom
  (ivy-use-virtual-buffers t)

  :config
  (ivy-mode 1))

(provide 'my-swiper)
;;; my-swiper.el ends here
