(in-package :rl)

;;;; Lippert ------------------------------------------------------------------
;;; https://blogs.msdn.microsoft.com/ericlippert/2011/12/12/shadowcasting-in-c-part-one/

(deftype coordinate ()
  '(integer 0 65000))

(deftype positive-coordinate ()
  '(integer 1 65000))


(defstruct (direction-vector (:constructor make-direction-vector (x y)))
  (x (required) :type positive-coordinate)
  (y (required) :type coordinate))

(defstruct (column-portion (:constructor make-column-portion (x top bottom)))
  (x (required) :type fixnum)
  (top (required) :type direction-vector)
  (bottom (required) :type direction-vector))


(define-with-macro column-portion x top bottom)
(define-with-macro direction-vector x y)


(defun make-upper-left-direction-vector (x y)
  (make-direction-vector (- (* 2 x) 1)
                         (+ (* 2 y) 1)))

(defun make-lower-right-direction-vector (x y)
  (make-direction-vector (+ (* 2 x) 1)
                         (+ (* 2 y) 1)))


(defun-inline make-queue-with (item)
  (let ((queue (make-queue)))
    (enqueue item queue)
    queue))

(defmacro doqueue ((queue-name initial-item) &body body)
  `(do ((,queue-name (make-queue-with ,initial-item)))
     ((queue-empty-p ,queue-name))
     ,@body))


(declaim (ftype (function (coordinate direction-vector) coordinate)
                topmost-visible-cell bottommost-visible-cell))


(defun in-radius-p (x y radius)
  (declare (optimize speed)
           (type coordinate x y radius))
  (<= (+ (square (- (* 2 x) 1))
         (square (- (* 2 y) 1)))
      (* 4 radius radius)))


(defun topmost-visible-cell (c top)
  (declare (optimize speed))
  (with-direction-vector (top)
    (multiple-value-bind (quotient remainder)
        (truncate (* (+ (* 2 c) 1) y)
                  (* 2 x))
      (if (<= remainder x)
        quotient
        (1+ quotient)))))

(defun bottommost-visible-cell (c bottom)
  (declare (optimize speed))
  (with-direction-vector (bottom)
    (multiple-value-bind (quotient remainder)
        (truncate (* (- (* 2 c) 1) y)
                  (* 2 x))
      (if (< remainder x)
        quotient
        (1+ quotient)))))


(defun translate-function-origin (function x y)
  (lambda (x% y%)
    (funcall function (+ x x%) (+ y y%))))

(defun translate-function-octant (function octant)
  (macrolet ((fn (new-x new-y)
               `(lambda (x y)
                  (funcall function ,new-x ,new-y))))
    (ecase octant
      (0 (fn x y))
      (1 (fn y x))
      (2 (fn (- y) x))
      (3 (fn (- x) y))
      (4 (fn (- x) (- y)))
      (5 (fn (- y) (- x)))
      (6 (fn y (- x)))
      (7 (fn x (- y))))))


(defun compute-fov-for-column-portion
    (radius queue x top bottom opaquep mark-visible)
  (iterate
    (for y
         :from (topmost-visible-cell x top)
         :downto (bottommost-visible-cell x bottom))

    (for in-radius? = (in-radius-p x y radius))
    (for opaque? = (or (not in-radius?) (funcall opaquep x y)))
    (for prev-opaque? :previous opaque?)

    (when in-radius?
      (funcall mark-visible x y))

    (if-first-time
      nil
      (cond
        ((and opaque? (not prev-opaque?))
         ;; transparent -> opaque
         (enqueue (make-column-portion (1+ x)
                                       top
                                       (make-upper-left-direction-vector x y))
                  queue))
        ((and (not opaque?) prev-opaque?)
         ;; opaque -> transparent
         (setf top (make-lower-right-direction-vector x y)))))
    (finally
      (when (not opaque?)
        (enqueue (make-column-portion (1+ x) top bottom)
                 queue)))))


(defun compute-fov-in-octant-zero (radius opaquep mark-visible)
  (funcall mark-visible 0 0)
  (doqueue (queue (make-column-portion 1
                                       (make-direction-vector 1 1)
                                       (make-direction-vector 1 0)))
    (with-column-portion ((dequeue queue))
      (unless (>= x radius)
        (compute-fov-for-column-portion radius queue x top bottom opaquep mark-visible)))))


(defun compute-fov (x y radius opaquep mark-visible)
  (iterate
    (with opaquep% = (translate-function-origin opaquep x y))
    (with mark-visible% = (translate-function-origin mark-visible x y))
    (for octant :from 0 :to 7)
    (compute-fov-in-octant-zero radius
                                (translate-function-octant opaquep% octant)
                                (translate-function-octant mark-visible% octant))))
