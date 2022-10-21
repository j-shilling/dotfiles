(define-module (jrs config development)
  #:use-module (rde features version-control)
  #:use-module (rde features docker)
  #:use-module (rde features clojure)
  #:use-module (rde features markup)

  #:use-module (jrs features javascript)

  #:use-module (jrs packages clojure-lsp)
  #:use-module (jrs packages node))

(define-public %development-features
  (list
   (feature-markdown)
   (feature-docker)
   (feature-clojure
    #:clojure-lsp
    clojure-lsp)
   (feature-javascript
    #:node-pkg node)
   (feature-git)))
