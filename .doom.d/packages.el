;; -*- no-byte-compile: t; -*-

(package! modus-themes)

(package! deferred)

(package! app-launcher
  :recipe
  (:host github :repo "SebastienWae/app-launcher"))

(package! git-timemachine)

(package! exec-path-from-shell)

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

;;
;; ELisp
;;

(package! elisp-lint)
(package! el-mock)

;;
;; JavaScript/Typescript
;;

(package! jest-test-mode
  :recipe
  (:host github :repo "rymndhng/jest-test-mode"
   :fork (:repo "j-shilling/jest-test-mode" :branch "dev")))

(package! nvm)
(package! add-node-modules-path)
