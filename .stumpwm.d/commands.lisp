(in-package :stumpwm-init)

(stumpwm:defcommand sly-start-server () ()
  "Start a slynk server for sly."
  (sb-thread:make-thread
   (lambda ()
     (slynk:create-server :dont-close t :port 4005))))

(stumpwm:defcommand sly-stop-server () ()
  "Stop current slynk server."
  (sb-thread:make-thread
   (slynk:stop-server 4005)))
