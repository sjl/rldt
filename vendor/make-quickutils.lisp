(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :with-gensyms
               :curry
               :rcurry
               :map-tree
               :ensure-boolean

               )
  :package "RL.QUICKUTILS")
