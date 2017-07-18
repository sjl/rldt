(in-package :rl.utils)

(defmacro dorange (ranges &body body)
  (if (null ranges)
    `(progn ,@body)
    (destructuring-bind (var from below) (first ranges)
      `(loop :for ,var :from ,from :below ,below
             :do (dorange ,(rest ranges) ,@body)))))

