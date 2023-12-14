(in-package :stumpwm-init)

(defvar +modus-vivendi-colors+
  `((,+modus-vivendi-bg-dim+ ,+modus-vivendi-bg-main+)
    (,+modus-vivendi-red+ ,+modus-vivendi-red-intense+)
    (,+modus-vivendi-green+ ,+modus-vivendi-green-intense+)
    (,+modus-vivendi-yellow+ ,+modus-vivendi-yellow-intense+)
    (,+modus-vivendi-blue+ ,+modus-vivendi-blue-intense+)
    (,+modus-vivendi-magenta+ ,+modus-vivendi-magenta-intense+)
    (,+modus-vivendi-cyan+ ,+modus-vivendi-cyan-intense+)
    (,+modus-vivendi-fg-dim+ ,+modus-vivendi-fg-main+))
  "List of colors for modus-vivendi theme. These should be assigned to *colors*.")

(defparameter *color-fg* +modus-vivendi-fg-main+)
(defparameter *color-bg* +modus-vivendi-bg-main+)
(defparameter *color-accent* +modus-vivendi-indigo+)

(defparameter *color-fg-active* *color-fg*)
(defparameter *color-bg-active* +modus-vivendi-bg-blue-subtle+)
(defparameter *color-fg-inactive* +modus-vivendi-fg-mode-line-inactive+)

(defparameter *color-bg-inactive* +modus-vivendi-bg-mode-line-inactive+)

(defparameter *color-border-focus* *color-accent*)
(defparameter *color-border-unfocus* +modus-vivendi-border+)

(defparameter *color-mode-line-fg* +modus-vivendi-fg-mode-line-inactive+)
(defparameter *color-mode-line-bg* +modus-vivendi-bg-mode-line-inactive+)
(defparameter *color-mode-line-info* +modus-vivendi-modeline-info+)

(defparameter *color-group-fg-active* *color-fg-active*)
(defparameter *color-group-bg-active* *color-bg-active*)

(defun set-modus-vivendi-colors ()
  (setq *colors* +modus-vivendi-colors+)

  (set-focus-color *color-border-focus*)
  (set-unfocus-color *color-border-unfocus*)

  (set-win-bg-color *color-bg*)

  (set-fg-color *color-fg*)
  (set-bg-color *color-bg*)
  (set-border-color *color-border-focus*)

  (setf *bar-med-color* "^B^8*")
  (setf *bar-hi-color* "^9*")
  (setf *bar-crit-color* "^B^9*")

  (setf *mode-line-background-color* *color-mode-line-bg*
        *mode-line-foreground-color* *color-mode-line-fg*
        *mode-line-border-color* *color-mode-line-bg*
        *mode-line-highlight-template*
        (concat "^["
                "^(:fg \"" *color-group-fg-active* "\")"
                "^(:bg \"" *color-group-bg-active* "\")"
                "~a^]"))
  (update-color-map (current-screen)))

(defun set-general-theme ()
  (set-modus-vivendi-colors)
  (set-normal-gravity :center)
  (set-maxsize-gravity :center)
  (set-transient-gravity :center)
  (gravity :center)

  ;; Message Windows
  (setf *input-window-gravity* :top
        *message-window-padding* 10
        *message-window-y-padding* 10
        *message-window-gravity* :top)
  (setf swm-gaps:*head-gaps-size* 0
      swm-gaps:*inner-gaps-size* 5
      swm-gaps:*outer-gaps-size* 5)
  (swm-gaps:toggle-gaps-on))

(when *initializing*
  (set-general-theme))
