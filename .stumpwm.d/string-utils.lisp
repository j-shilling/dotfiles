(in-package :stumpwm-init)

(defun bold-string (str)
  (concat +font-bold+ str +font-main+))

(defun icon-string (str)
  (concat +font-icon+ str +font-main+))

(defun parse-color (color)
  (when (stringp color)
    (if (= 1 (length color))
        color
        (concat "\"" color "\""))))

(defun parsed-color-to-prop-string (id color)
  (when color
    (concat "^(:" id " " color ")")))

(defun color-string (str fg bg)
  (let ((fc (parsed-color-to-prop-string
             "fg" (parse-color fg)))
        (bc (parsed-color-to-prop-string
             "bg" (parse-color bg))))
    (concat "^[" fc bc str "^]")))

(defun color-icon (icon fg bg)
  (icon-string (color-string icon fg bg)))
