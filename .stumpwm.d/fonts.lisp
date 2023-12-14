(in-package :stumpwm-init)

(xft:cache-fonts)

(set-font
 `(,(make-instance 'xft:font
                   :family "Fira Code"
                   :subfamily "Regular"
                   :size 14
                   :antialias t)
    ,(make-instance 'xft:font
                   :family "Fira Code"
                   :subfamily "Bold"
                   :size 14
                   :antialias t)
    ,(make-instance 'xft:font
                    :family "FontAwesome"
                    :subfamily "Regular"
                    :size 14
                    :antialias t)))

(defparameter +font-main+ "^f0")
(defparameter +font-bold+ "^f1")
(defparameter +font-icon+ "^f1")
