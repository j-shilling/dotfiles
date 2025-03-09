(add-to-load-path "/home/jake/dotfiles/src")

(use-modules (guix profiles)
             (gnu packages)
             (config packages node-xyz))

(define pnpm
  pnpm-10.6.1)

(define node
  (specification->package "node@22"))

(packages->manifest
 (list pnpm node))
