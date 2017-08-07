(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :conjoin
               :curry
               :disjoin
               :ensure-boolean
               :map-tree
               :once-only
               :rcurry
               :removef
               :with-gensyms

               )
  :package "RL.QUICKUTILS")
