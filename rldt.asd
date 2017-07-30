(asdf:defsystem :rldt
  :description "r/RoguelikeDev does the complete roguelike tutorial"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"

  :depends-on (:beast
               :bordeaux-threads
               :cl-blt
               :cl-pcg
               :iterate
               :losh
               :pileup)

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components
                ((:file "utils")
                 (:file "field-of-view")
                 (:file "binary-space-partitioning")
                 (:file "a-star")
                 (:file "panels")
                 (:file "main")))))
