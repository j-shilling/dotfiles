(define-module (config packages tree-sitter)
  #:use-module (gnu packages tree-sitter)
  #:use-module (guix gexp)
  #:use-module (guix git-download))

(define tree-sitter-grammar
  (module-ref
   (resolve-module '(gnu packages tree-sitter))
   'tree-sitter-grammar))

(define-public tree-sitter-astro
  (let ((commit "6e3bad36a8c12d579e73ed4f05676141a4ccf68d")
        (revision "1")
        (version "0.0.1"))
    (tree-sitter-grammar
     "astro" "ASTRO"
     "1238n529jipzrchkd1z8y3rn9qcascykls8h128qz2322x0la25rf"
     (git-version version revision commit)
     #:commit commit
     #:repository-url "https://github.com/virchau13/tree-sitter-astro.git"
     #:inputs (list tree-sitter-typescript
                    tree-sitter-javascript
                    tree-sitter-css
                    tree-sitter-html)
     #:get-cleanup-snippet
     (lambda _
       #~(begin
           (use-modules (guix build utils))
           (delete-file "binding.gyp")
           (delete-file-recursively "bindings"))))))

tree-sitter-astro
