(use-package! app-launcher
  :bind
  ("s-d" . app-launcher-run-app))

(use-package! exwm
  :init
  (require 'exwm)
  (require 'exwm-randr)
  (require 'map)
  (require 'cl-lib)
  (require 'cl-seq)
  (require 'monitors)

  (let ((setup-script "~/.screenlayout/xrandr-setup.sh"))
    (when (file-exists-p setup-script)
      (start-file-process-shell-command "xrandr-setup" nil setup-script)))

  (let ((monitors (monitors-get-connected)))
    (setq exwm-workspace-number (seq-length monitors)
          exwm-randr-workspace-monitor-plist
          (let ((workspace-id 1))
            (cl-reduce (lambda (acc el)
                         (let ((new-acc (plist-put acc workspace-id (monitors-monitor-name el))))
                           (cl-incf workspace-id)
                           new-acc))
                       monitors
                       :initial-value nil))))

  (setq exwm-workspace-show-all-buffers t)
  (setq focus-follows-mouse t)

  (add-hook 'exwm-init-hook
            (lambda ()
              (dolist (prog '("nm-applet" "pasystray" "blueman-applet"))
                (call-process prog nil 0 nil))))

  (add-hook 'exwm-init-hook
            #'polybar-start-all)

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (defun exwm-polybar-workspace-name ()
    (apply exwm-workspace-index-map `(,exwm-workspace-current-index)))
  (add-hook 'exwm-workspace-switch-hook
            (lambda ()
              (polybar-send-msg "exwm-workspace" 1)))
  (defun exwm-polybar-window-name ()
    (or exwm-title
        exwm-class-name
        (with-current-buffer (window-buffer (frame-selected-window))
          (buffer-name))))
  (advice-add 'switch-to-buffer :after
            (lambda (&rest r)
              (polybar-send-msg "exwm-window" 1)))
  (advice-add 'select-window :after
            (lambda (&rest r)
              (polybar-send-msg "exwm-window" 1)))

  (unless (get 'exwm-input-global-keys 'saved-value)
    (setq exwm-input-global-keys
          `(
            ([?\M-x] . execute-extended-command)
            ;; 's-r': Reset (to line-mode).
            ([?\s-r] . exwm-reset)
            ;; 's-w': Switch workspace.
            ([?\s-w] . exwm-workspace-switch)
            ;; 's-&': Launch application.
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            ;; 's-N': Switch to certain workspace.
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 1 exwm-workspace-number)))))

  (exwm-randr-enable)
  (exwm-enable))
