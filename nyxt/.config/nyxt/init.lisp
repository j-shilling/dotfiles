(in-package #:nyxt-user)

;;; Load quicklisp. Not sure it works.
#-quicklisp
(let ((quicklisp-init
        (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :slynk)
(load-after-system :slynk (nyxt-init-file "slynk.lisp"))

(define-configuration browser
    ((session-restore-prompt :never-restore)
     (external-editor-program
      (list "emacsclient" "-cn" "-a" ""))))

(define-configuration (buffer prompt-buffer)
  ((default-modes `(nyxt/emacs-mode:emacs-mode ,@%slot-default%))
   (download-engine :renderer)
   (current-zoom-ratio 1)))

(define-configuration prompt-buffer
  ((override-map (let ((map (make-keymap "override-map")))
                   (define-key map
                     "escape" 'nyxt/prompt-buffer-mode:cancel-input
                     "C-g" 'nyxt/prompt-buffer-mode:cancel-input)
                   map))))

(define-configuration web-buffer
  ((default-modes `(nyxt/emacs-mode:emacs-mode
                    auto-mode
                    blocker-mode
                    force-https-mode
                    reduce-tracking-mode
                    firefox-mimick-mode
                    ,@%slot-default%))))

;;; reduce-tracking-mode has a preferred-user-agent slot that it uses
;;; as the User Agent to set when enabled. What I want here is to have
;;; the same thing as reduce-tracking-mode, but with a different User
;;; Agent.
(define-mode chrome-mimick-mode (nyxt/reduce-tracking-mode:reduce-tracking-mode)
  "A simple mode to set Chrome-like Windows user agent."
  ((nyxt/reduce-tracking-mode:preferred-user-agent
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/95.0.4638.69 Safari/537.36")))

(define-mode firefox-mimick-mode (nyxt/reduce-tracking-mode:reduce-tracking-mode)
  "A simple mode to set Firefox-like Linux user agent."
  ((nyxt/reduce-tracking-mode:preferred-user-agent
    "Mozilla/5.0 (X11; Linux x86_64; rv:94.0) Gecko/20100101 Firefox/94.0")))
