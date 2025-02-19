(use-modules (ares server)
             (ice-9 popen)
             (ice-9 textual-ports)
             (rnrs io ports))

(define root
  (canonicalize-path
   (string-trim-right
    (get-string-all
     (open-pipe* OPEN_READ "git" "rev-parse" "--show-toplevel")))))

(add-to-load-path
 (canonicalize-path
  (string-append root "/src")))

(run-nrepl-server #:nrepl-port-path "./.nrepl-port")
