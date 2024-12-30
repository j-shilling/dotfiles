(define-module (config packages lisp-xyz)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages lisp-check))

(define-public sbcl-cl-transducers
  (let ((commit "9b424bd7a2eec7ad4bc258501f28170c6f00eaf2")
        (revision "0"))
    (package
      (name "sbcl-cl-transducers")
      (version (git-version "1.3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fosskers/cl-transducers")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0p7fqkmv0rfi5d7mmm9600qpix003bqr7as148pk157s1d44vncg"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:asd-systems '("transducers" "transducers/jzon" "transducers/fset")))
      (inputs
       (list
        sbcl-cl-str
        sbcl-parachute))
      (propagated-inputs
       (list
        sbcl-fset
        sbcl-trivia
        sbcl-jzon))
      (home-page "https://fosskers.github.io/cl-transducers/")
      (synopsis "Transducers: Ergonomic, efficient data processing")
      (description "Transducers are an ergonomic and extremely memory-efficient way to process a
data source. Here \"data source\" means simple collections like Lists or
Vectors, but also potentially large files or generators of infinite data.")
      (license license:expat))))

sbcl-cl-transducers
