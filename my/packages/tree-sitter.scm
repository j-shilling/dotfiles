(define-module (config packages tree-sitter)
  #:use-module (guix build-system tree-sitter)
  #:use-module (gnu packages tree-sitter)
  #:use-module (guix gexp)
  #:use-module (guix git-download))

(define tree-sitter-grammar
  (module-ref
   (resolve-module '(gnu packages tree-sitter))
   'tree-sitter-grammar))

(define-public tree-sitter-astro
  (let ((commit "0ad33e32ae9726e151d16ca20ba3e507ff65e01f")
        (revision "2")
        (version "0.0.1")
        (repository-url "https://github.com/virchau13/tree-sitter-astro.git")
        (hash "1238n529jipzrchkd1z8y3rn9qcascykls8h128qz2322x0la25rf"))
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
     (lambda (grammar-directories)
       #~(begin
           (use-modules (guix build utils))
           (delete-file "tree-sitter-javascript.wasm")
           (delete-file "binding.gyp")
           (delete-file-recursively "bindings")
           (for-each
            (lambda (lang)
              (with-directory-excursion lang
                (delete-file "src/grammar.json")
                (delete-file "src/node-types.json")
                (delete-file "src/parser.c")
                (delete-file "src/tree_sitter/parser.h")))
            '#$grammar-directories))))))

(define-public tree-sitter-yaml
  (tree-sitter-grammar
   "yaml" "Yaml"
   "1ay4snkd2s4pid7pcr4bgx0y9cj7b5vlgd7wfc1j0896l0p61cjb"
   "0.5.0"
   #:repository-url "https://github.com/emturner/tree-sitter-yaml"
   #:commit "f53859209fef065b677995802842a2b65314defb"))
