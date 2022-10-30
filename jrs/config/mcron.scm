(define-module (jrs config mcron)
  #:use-module (guix gexp)
  #:export (%mcron-jobs))

(define %mcron-jobs
  (list
   #~(job '(next-minute
            (range 0 60 10))
          (lambda ()
            (system* "mbsync" "-Va")
            (system* "notmuch" "new")))))
