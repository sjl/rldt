(ql:quickload 'rldt)

(cffi:close-foreign-library 'blt:bearlibterminal)

(sb-ext:gc :full t)

(sb-ext:save-lisp-and-die
  "build/rldt-mac"
  :toplevel #'rl:main-mac
  :executable t)
