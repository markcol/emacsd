;;; my-rand.el --- rand configuration.  -*- lexical-binding: t ; -*-

;;; Commentary:

;; Basic configuration for rand.

;;; Code:

;; Borrowed from:
;; http://ergoemacs.org/emacs/elisp_insert_random_number_string.html

;; seed random number
(random t)

(defun my/rand-alphanumeric (NUM)
  "Insert a random alphanumeric string of length NUM."
  (interactive "P")
  (let* ((charset (concat "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                          "abcdefghijklmnopqrstuvwxyz"
                          "0123456789"))
         (baseCount (length charset)))
    (dotimes (_ (if (numberp NUM) (abs NUM) 16))
      (insert (elt charset (random baseCount))))))

(defun my/rand-hex (NUM)
  "Insert NUM random hexadecimal digits."
  (interactive "P")
  (let ((n (if (numberp NUM) (abs NUM) 6 )))
    (insert (format
             (concat "%0" (number-to-string n) "x" )
             (random (1- (expt 16 n)))))))

(defun my/rand-num (NUM)
  "Insert NUM random digits."
  (interactive "P")
  (let ((charset "1234567890")
        (baseCount 10))
    (dotimes (_ (if (numberp NUM) (abs NUM) 16 ))
      (insert (elt charset (random baseCount))))))

(defun my/rand-ip ()
  "Insert a random IPv4 address."
  (interactive)
  (insert (concat (int-to-string (random 255))
                  "." (int-to-string (random 255))
                  "." (int-to-string (random 255))
                  "." (int-to-string (random 255)))))

(provide 'my-rand)
;;; my-rand.el ends here
