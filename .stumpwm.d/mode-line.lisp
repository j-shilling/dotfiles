(in-package :stumpwm-init)

(setf *mode-line-position* :top
      *mode-line-border-width* 0
      *mode-line-pad-x* 5)

(setf *mode-line-timeout* 1)

(setf *time-modeline-string* "%l:%M %a %e %b")

(setf *group-format* "%t")

(setf *window-format*
      (concat "%s "
              (color-string "%c %30t" *color-fg-inactive* *color-bg-inactive*)))

(setf *screen-mode-line-format*
      (list
       "%W"
       "^>"
       "%d"
       "%T"))

(when *initializing*
  (dolist (head
           (screen-heads (current-screen)))
    (enable-mode-line (current-screen) head
                      t *screen-mode-line-format*)))
