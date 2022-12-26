(define-module (jrs features python)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home-services shells)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tcl)
  #:export (feature-python))

(define (feature-python)

  (define (home-python-services config)
    (let ((emacs-f-name 'python)
          (pyvenv-init "\
export PYENV_ROOT=\"$HOME/.pyenv\"

if [ -d \"${PYENV_ROOT}\" ] ; then
     command -v pyenv >/dev/null || export PATH=\"$PYENV_ROOT/bin:$PATH\"
     eval \"$(pyenv init -)\"
fi"))
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

       (when (get-value 'zsh config)
         (simple-service
          'pyvenv-zsh-init
          home-zsh-service-type
          (home-zsh-extension
           (zshrc
            (list
             pyvenv-init)))))

       (when (get-value 'bash config)
         (simple-service
          'pyvenv-bash-init
          home-bash-service-type
          (home-bash-extension
           (bashrc
            (list
             pyvenv-init)))))

       (simple-service
        'packages-for-python
        home-profile-service-type
        (list python-lsp-server
              python
              ;; for pyenv to build python
              gcc
              bzip2
              expat
              gdbm
              libffi ; for ctypes
              sqlite ; for sqlite extension
              openssl
              readline
              zlib
              tcl
              tk
              xz)))))

  (feature
   (name 'python)
   (home-services-getter home-python-services)))
