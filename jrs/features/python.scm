(define-module (jrs features python)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:export (feature-python))

(define (feature-python)

  (define (home-python-services config)
    (let ((emacs-f-name 'python))
      (list
       (rde-elisp-configuration-service
        emacs-f-name
        config
        '((add-hook 'python-mode-hook 'eglot)
          (add-to-list 'auto-mode-alist
                       '("[./]flake8\\'" . conf-mode))
          (add-to-list 'auto-mode-alist
                       '("/Pipefile\\'" . conf-mode))
          (with-eval-after-load
           'pyvenv
           (add-hook 'python-mode-hook 'pyvenv-mode)
           (add-hook 'pyvenv-mode-hook 'pyvenv-tracking-mode)
           (add-to-list 'global-mode-string
                        '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))
                        'append)))
        #:elisp-packages
        (list emacs-pyvenv
              emacs-pyimport
              emacs-py-isort))

       (simple-service
        'packages-for-python
        home-profile-service-type
        (list python-lsp-server)))))

  (feature
   (name 'python)
   (home-services-getter home-python-services)))
