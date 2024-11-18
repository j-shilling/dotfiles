(define-module (config home services shell)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu packages readline)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:export (shell-services))

(define shell-packages
  (list readline))

(define shell-packages-service
  (simple-service 'readline-service
                  home-profile-service-type
                  shell-packages))

(define inputrc-service
  (service home-inputrc-service-type
            (home-inputrc-configuration
             (key-bindings
              `())
             (variables
              `(("bell-style" . "none")
                ("bind-tty-special-chars" . #t)
                ("blink-matching-paren". #t)
                ("colored-completion-prefix" . #t)
                ("colored-stats" . #t)
                ("convert-meta" . #f)
                ("editing-mode" . "emacs")
                ("enable-bracketed-paste" . #f)
                ("expand-tilde". #t)
                ("keymap" . "emacs")
                ("mark-symlinked-directories" . #t)
                ("match-hidden-files" . #t)
                ("meta-flag" . #t)
                ("output-meta" . #t)
                ("show-all-if-ambiguous" . #t)
                ("show-mode-in-prompt" . #f)))
             (conditional-constructs
              `(("$include" . "/etc/inputrc")
                ("$include" . ,(file-append readline "/etc/inputrc")))))))

(define bash-service
  (service home-bash-service-type
           (home-bash-configuration)))

(define shell-services
  (list
   shell-packages-service
   inputrc-service
   bash-service))
