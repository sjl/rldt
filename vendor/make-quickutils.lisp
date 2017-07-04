(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :with-gensyms
               :curry
               :rcurry

               )
  :package "RL.QUICKUTILS")
