(asdf:defsystem #:stumpwm-init
  :description "StumpWM init"
  :depends-on (:legit
               :zpng
               :dbus
               :xembed
               :slynk
               :stumpwm)
  :serial t
  :components ((:file "package")
               (:file "modules")
               (:file "colors")
               (:file "commands")
               (:file "fonts")
               (:file "string-utils")
               (:file "keybindings")
               (:file "placement")
               (:file "theme")
               (:file "stumpwm-init")
               (:file "mode-line")))
