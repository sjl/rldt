(in-package :rl.utils)

(defmacro dorange (ranges &body body)
  (if (null ranges)
    `(progn ,@body)
    (destructuring-bind (var from below) (first ranges)
      `(loop :for ,var :from ,from :below ,below
             :do (dorange ,(rest ranges) ,@body)))))

(defmacro dorepeat (n &body body)
  `(dotimes (,(gensym) ,n)
     ,@body))


(defmacro sortf (place-1 place-2 &key (key ''<))
  (with-gensyms (value-1 value-2)
    `(let ((,value-1 ,place-1)
           (,value-2 ,place-2))
       (when (funcall ,key ,value-2 ,value-1)
         (rotatef ,place-1 ,place-2)))))
