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

(define sbcl-cl-csv ;; TODO contribute upstream
  (let ((commit "2d64d4183bfc91824068cd4cf3414238d3c00fe5")
        (revision "0"))
    (package
      (name "sbcl-cl-csv")
      (version (git-version "1.0.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AccelerationNet/cl-csv")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0pb89l3bi2cnk7sav2w0dmlvjxij1wpy3w6n9c4b6imjs0pznrxi"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:asd-systems '("cl-csv")))
      (inputs
       (list
        sbcl-iterate
        sbcl-alexandria))
      (propagated-inputs
       (list
        sbcl-lisp-unit2))
      (home-page "https://github.com/AccelerationNet/cl-csv")
      (synopsis "A common lisp library providing easy csv reading and writing")
      (description "This library aims to simplify working with csvs to the bare minimum of
tedium

- reads/writes csvs from/to strings, streams and files

- support streaming reads (allowing processing very large csvs,
through read-csv's row-fn paramter)

- supports custom data formating

- settable quote, separator and quote-escapes

- supports multiline quoted data

- A test suite

- Detailed state about the process on error (line number, column
number, char index), current collection state")
      (license license:bsd-4))))


sbcl-cl-csv
