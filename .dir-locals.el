;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((scheme-mode
  . ((fill-column . 78)
     (tab-width . 8)
     (sentence-end-double-space . t)
     (geiser-insert-actual-lambda . nil)
     (eval . (setq geiser-guile-load-path (let ((root (project-root (project-current))))
                                            (list
                                             (format "%s/target/profiles/guix/share/guile/site/3.0" root)
                                             (format "%s/src" root))))))))
