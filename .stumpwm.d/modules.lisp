(in-package :stumpwm-init)

(defvar +stumpwm-contrib-repo+
  "https://github.com/stumpwm/stumpwm-contrib"
  "The URL of the stumpwm-contrib repository.")

(defvar +stumpwm-pamixer-repo+
  "https://github.com/Junker/stumpwm-pamixer.git"
  "The URL of the stumpwm-pamixer repository.")

;; Clone the contrib repo if its not already cloned.
(unless (probe-file *module-dir*)
  (legit:clone modules-repo *module-dir*)
  (init-load-path *module-dir*))

;; Ensure pamixer module is cloned.
(let ((pamixer-dir (merge-pathnames "pamixer" *module-dir*)))
  (unless (probe-file pamixer-dir)
    (legit:clone +stumpwm-pamixer-repo+ pamixer-dir)
    (add-to-load-path pamixer-dir)))

;; clx-truetype is not in quicklisp for some reason
(let ((clx-truetype-repo "https://github.com/lihebi/clx-truetype.git")
      (clx-truetype-dir (truename "~/quicklisp/local-projects/clx-truetype")))
  (unless (probe-file clx-truetype-dir)
    (legit:clone clx-truetype-repo clx-truetype-dir))
  (pushnew clx-truetype-dir ql:*local-project-directories*)
(ql:register-local-projects)
  (ql:quickload :clx-truetype))

;; Load the contrib modules.
(load-module "ttf-fonts")
(load-module "swm-gaps")
;; (load-module "pamixer")
;; (load-module "cpu")
;; (load-module "mem")
;; (load-module "wifi")
;; (load-module "screenshot")
;; (load-module "stump-lock")
;; (load-module "stump-nm")
;; (load-module "stump-emacs")
;; (load-module "stump-ssh")
;; (load-module "winner-mode")
(load-module "stumptray")

(provide 'stumpwm-init-modules)
