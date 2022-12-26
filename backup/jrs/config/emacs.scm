(define-module (jrs config emacs)
  #:use-module (gnu packages)

  #:use-module (guix gexp)

  #:use-module (rde gexp)

  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features terminals))

(define-public %emacs-features
  (list
   (feature-emacs
    #:additional-elisp-packages
    (map specification->package+output
         '("emacs-paredit"
           "emacs-pinentry"
           "emacs-circe"))
    #:extra-init-el
    `(,(slurp-file-like (local-file "../../elisp/configure-defaults.el"))
      ,(slurp-file-like (local-file "../../elisp/configure-lisp.el"))
      (setq circe-network-options
            '(("Libera Chat"
               :host "irc.libera.chat"
               :port 6697
               :nick "Jacobissimus"
               :pass (lambda (&rest _) (password-store-get "irc/libera.chat"))
               :channels ("#guix" "#emacs"))))))

   (feature-emacs-appearance
    #:dark? #t)
   (feature-emacs-faces)
   (feature-emacs-tramp)
   (feature-emacs-completion)
   (feature-emacs-corfu)
   (feature-emacs-vertico)
   (feature-emacs-project)
   (feature-emacs-which-key)
   (feature-emacs-keycast
    #:turn-on? #f)
   (feature-emacs-dired)
   (feature-emacs-eshell)
   (feature-vterm)
   (feature-emacs-git)
   (feature-emacs-geiser)
   (feature-emacs-guix)
   (feature-emacs-eglot)

   (feature-emacs-spelling
    #:flyspell-hooks
    '(text-mode-hook)
    #:flyspell-prog-hooks
    '(prog-mode-hook))

   (feature-emacs-pdf-tools)
   (feature-emacs-org
    #:org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
    #:org-capture-templates
    '(("t" "Todo [inbox]" entry
       (file+headline "~/org/agenda/inbox.org" "Tasks")
       "* TODO %i%?")
      ("T" "Tickler" entry
       (file+headling "~/org/agenda/tickler.org" "Tickler")
       "% %i%? \n %U")))
   (feature-emacs-org-agenda
    #:org-agenda-files
    '("~/org/agenda/inbox.org"
      "~/org/agenda/gtd.org"
      "~/org/agenda/someday.org"
      "~/org/agenda/tickler.org"))
   (feature-emacs-org-roam
    #:org-roam-directory
    (string-append (getenv "XDG_STATE_HOME")
                   "/emacs/org-roam")
    #:org-roam-dailies-directory
    (string-append (getenv "XDG_STATE_HOME")
                   "/emacs/org-roam-dailies"))))
