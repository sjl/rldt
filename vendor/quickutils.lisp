;;;; This file was automatically generated by Quickutil.
;;;; See http://quickutil.org for details.

;;;; To regenerate:
;;;; (qtlc:save-utils-as "quickutils.lisp" :utilities '(:CONJOIN :CURRY :DISJOIN :ENSURE-BOOLEAN :MAP-TREE :ONCE-ONLY :RCURRY :REMOVEF :WITH-GENSYMS) :ensure-package T :package "RL.QUICKUTILS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "RL.QUICKUTILS")
    (defpackage "RL.QUICKUTILS"
      (:documentation "Package that contains Quickutil utility functions.")
      (:use #:cl))))

(in-package "RL.QUICKUTILS")

(when (boundp '*utilities*)
  (setf *utilities* (union *utilities* '(:CONJOIN :MAKE-GENSYM-LIST
                                         :ENSURE-FUNCTION :CURRY :DISJOIN
                                         :ENSURE-BOOLEAN :MAP-TREE :ONCE-ONLY
                                         :RCURRY :REMOVEF :STRING-DESIGNATOR
                                         :WITH-GENSYMS))))

  (defun conjoin (predicate &rest more-predicates)
    "Returns a function that applies each of `predicate` and `more-predicate`
functions in turn to its arguments, returning `nil` if any of the predicates
returns false, without calling the remaining predicates. If none of the
predicates returns false, returns the primary value of the last predicate."
    (if (null more-predicates)
        predicate
        (lambda (&rest arguments)
          (and (apply predicate arguments)
               ;; Cannot simply use CL:EVERY because we want to return the
               ;; non-NIL value of the last predicate if all succeed.
               (do ((tail (cdr more-predicates) (cdr tail))
                    (head (car more-predicates) (car tail)))
                   ((not tail)
                    (apply head arguments))
                 (unless (apply head arguments)
                   (return nil)))))))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-gensym-list (length &optional (x "G"))
    "Returns a list of `length` gensyms, each generated as if with a call to `make-gensym`,
using the second (optional, defaulting to `\"G\"`) argument."
    (let ((g (if (typep x '(integer 0)) x (string x))))
      (loop repeat length
            collect (gensym g))))
  )                                        ; eval-when
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; To propagate return type and allow the compiler to eliminate the IF when
  ;;; it is known if the argument is function or not.
  (declaim (inline ensure-function))

  (declaim (ftype (function (t) (values function &optional))
                  ensure-function))
  (defun ensure-function (function-designator)
    "Returns the function designated by `function-designator`:
if `function-designator` is a function, it is returned, otherwise
it must be a function name and its `fdefinition` is returned."
    (if (functionp function-designator)
        function-designator
        (fdefinition function-designator)))
  )                                        ; eval-when

  (defun curry (function &rest arguments)
    "Returns a function that applies `arguments` and the arguments
it is called with to `function`."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (let ((fn (ensure-function function)))
      (lambda (&rest more)
        (declare (dynamic-extent more))
        ;; Using M-V-C we don't need to append the arguments.
        (multiple-value-call fn (values-list arguments) (values-list more)))))

  (define-compiler-macro curry (function &rest arguments)
    (let ((curries (make-gensym-list (length arguments) "CURRY"))
          (fun (gensym "FUN")))
      `(let ((,fun (ensure-function ,function))
             ,@(mapcar #'list curries arguments))
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest more)
           (apply ,fun ,@curries more)))))
  

  (defun disjoin (predicate &rest more-predicates)
    "Returns a function that applies each of `predicate` and
`more-predicate` functions in turn to its arguments, returning the
primary value of the first predicate that returns true, without
calling the remaining predicates. If none of the predicates returns
true, `nil` is returned."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (let ((predicate (ensure-function predicate))
          (more-predicates (mapcar #'ensure-function more-predicates)))
      (lambda (&rest arguments)
        (or (apply predicate arguments)
            (some (lambda (p)
                    (declare (type function p))
                    (apply p arguments))
                  more-predicates)))))
  

  (defun ensure-boolean (x)
    "Convert `x` into a Boolean value."
    (and x t))
  

  (defun map-tree (function tree)
    "Map `function` to each of the leave of `tree`."
    (check-type tree cons)
    (labels ((rec (tree)
               (cond
                 ((null tree) nil)
                 ((atom tree) (funcall function tree))
                 ((consp tree)
                  (cons (rec (car tree))
                        (rec (cdr tree)))))))
      (rec tree)))
  

  (defmacro once-only (specs &body forms)
    "Evaluates `forms` with symbols specified in `specs` rebound to temporary
variables, ensuring that each initform is evaluated only once.

Each of `specs` must either be a symbol naming the variable to be rebound, or of
the form:

    (symbol initform)

Bare symbols in `specs` are equivalent to

    (symbol symbol)

Example:

    (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
      (let ((y 0)) (cons1 (incf y))) => (1 . 1)"
    (let ((gensyms (make-gensym-list (length specs) "ONCE-ONLY"))
          (names-and-forms (mapcar (lambda (spec)
                                     (etypecase spec
                                       (list
                                        (destructuring-bind (name form) spec
                                          (cons name form)))
                                       (symbol
                                        (cons spec spec))))
                                   specs)))
      ;; bind in user-macro
      `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string (car n)))))
              gensyms names-and-forms)
         ;; bind in final expansion
         `(let (,,@(mapcar (lambda (g n)
                             ``(,,g ,,(cdr n)))
                           gensyms names-and-forms))
            ;; bind in user-macro
            ,(let ,(mapcar (lambda (n g) (list (car n) g))
                    names-and-forms gensyms)
               ,@forms)))))
  

  (defun rcurry (function &rest arguments)
    "Returns a function that applies the arguments it is called
with and `arguments` to `function`."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (let ((fn (ensure-function function)))
      (lambda (&rest more)
        (declare (dynamic-extent more))
        (multiple-value-call fn (values-list more) (values-list arguments)))))
  

  (declaim (inline remove/swapped-arguments))
  (defun remove/swapped-arguments (sequence item &rest keyword-arguments)
    (apply #'remove item sequence keyword-arguments))

  (define-modify-macro removef (item &rest remove-keywords)
    remove/swapped-arguments
    "Modify-macro for `remove`. Sets place designated by the first argument to
the result of calling `remove` with `item`, place, and the `keyword-arguments`.")
  

  (deftype string-designator ()
    "A string designator type. A string designator is either a string, a symbol,
or a character."
    `(or symbol string character))
  

  (defmacro with-gensyms (names &body forms)
    "Binds each variable named by a symbol in `names` to a unique symbol around
`forms`. Each of `names` must either be either a symbol, or of the form:

    (symbol string-designator)

Bare symbols appearing in `names` are equivalent to:

    (symbol symbol)

The string-designator is used as the argument to `gensym` when constructing the
unique symbol the named variable will be bound to."
    `(let ,(mapcar (lambda (name)
                     (multiple-value-bind (symbol string)
                         (etypecase name
                           (symbol
                            (values name (symbol-name name)))
                           ((cons symbol (cons string-designator null))
                            (values (first name) (string (second name)))))
                       `(,symbol (gensym ,string))))
            names)
       ,@forms))

  (defmacro with-unique-names (names &body forms)
    "Binds each variable named by a symbol in `names` to a unique symbol around
`forms`. Each of `names` must either be either a symbol, or of the form:

    (symbol string-designator)

Bare symbols appearing in `names` are equivalent to:

    (symbol symbol)

The string-designator is used as the argument to `gensym` when constructing the
unique symbol the named variable will be bound to."
    `(with-gensyms ,names ,@forms))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(conjoin curry disjoin ensure-boolean map-tree once-only rcurry
            removef with-gensyms with-unique-names)))

;;;; END OF quickutils.lisp ;;;;
