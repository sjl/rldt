(ql:quickload 'rldt)

(sb-ext:gc :full t)

(sb-ext:save-lisp-and-die
  "build/rldt"
  :toplevel #'rl:main
  :executable t)
