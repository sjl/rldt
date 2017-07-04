(in-package :rl.bsp)

(defmacro sortf (place-1 place-2 &key (key ''<))
  (with-gensyms (value-1 value-2)
    `(let ((,value-1 ,place-1)
           (,value-2 ,place-2))
       (when (funcall ,key ,value-2 ,value-1)
         (rotatef ,place-1 ,place-2)))))


(defparameter *minimum-room-size* 6)


(defstruct (area (:constructor make-area (x y w h)))
  x y w h)

(define-with-macro area x y w h)


(defun split (space divider)
  (let ((left (truncate (* space divider))))
    (values left (- space left))))


(defun partition-space (width height)
  (recursively ((x 0)
                (y 0)
                (w width)
                (h height))
    (flet ((split-horizontally (top bottom)
             (cons (recur x y w top)
                   (recur x (+ y top) w bottom)))
           (split-vertically (left right)
             (cons (recur x y left h)
                   (recur (+ x left) y right h))))
      (let ((n (random-range 0.4 0.6)))
        (multiple-value-bind* (((left right) (split w n))
                               ((top bottom) (split h n)))
          (let ((h? (>= (min top bottom) (* 1.3 *minimum-room-size*)))
                (v? (>= (min left right) (* 1.3 *minimum-room-size*))))
            (cond
              ((and h? v?) (if (randomp)
                             (split-horizontally top bottom)
                             (split-vertically left right)))
              (h? (split-horizontally top bottom))
              (v? (split-vertically left right))
              (t (make-area x y w h)))))))))

(defun place-rooms (tree)
  (map-tree (lambda (area)
              (with-area (area)
                (let* ((rw (random-range *minimum-room-size* w))
                       (rh (random-range *minimum-room-size* h))
                       (rx (+ x (random-range 0 (- w rw))))
                       (ry (+ y (random-range 0 (- h rh)))))
                  (make-area rx ry rw rh))))
            tree))

(defun make-partitioning (width height)
  (place-rooms (partition-space width height)))

(defun flatten-partitioning (tree)
  (recursively ((tree tree))
    (etypecase tree
      (area (list tree))
      (cons (append (recur (car tree))
                    (recur (cdr tree)))))))


;;;; Carving ------------------------------------------------------------------
(defun carve-room (carve area)
  (with-area (area)
    (iterate (for-nested ((x% :from (1+ x) :below (+ x w -2))
                          (y% :from (1+ y) :below (+ y h -2))))
      (funcall carve x% y%))))

(defun carve-horizontal-tunnel (carve x1 x2 y)
  (sortf x1 x2)
  (iterate (for x :from x1 :to x2)
           (funcall carve x y)))

(defun carve-vertical-tunnel (carve x y1 y2)
  (sortf y1 y2)
  (iterate (for y :from y1 :to y2)
           (funcall carve x y)))

(defun carve-tunnel (carve x1 y1 x2 y2)
  (if (randomp)
    (progn (carve-horizontal-tunnel carve x1 x2 y1)
           (carve-vertical-tunnel carve x2 y1 y2))
    (progn (carve-vertical-tunnel carve x1 y1 y2)
           (carve-horizontal-tunnel carve x1 x2 y2))))

(defun random-room-point (area)
  (with-area (area)
    (values (random-range (1+ x) (+ x w -2))
            (random-range (1+ y) (+ y h -2)))))

(defun carve-tunnels (carve tree)
  (recursively ((tree tree))
    (etypecase tree
      (area (list tree))
      (cons (destructuring-bind (a . b) tree
              (let* ((as (recur a))
                     (bs (recur b))
                     (room-a (random-elt as))
                     (room-b (random-elt bs)))
                (multiple-value-call #'carve-tunnel carve
                  (random-room-point room-a)
                  (random-room-point room-b))
                (append as bs)))))))

(defun carve-rooms (carve tree)
  (map-tree (curry #'carve-room carve) tree))

(defun carve-partitioning (carve tree)
  (carve-rooms carve tree)
  (carve-tunnels carve tree))


;;;; Debug ---------------------------------------------------------------------
(defun random-color (alpha)
  (blt:hsva (random 1.0) (random-range 0.6 1.0) (random-range 0.5 1.0)
            alpha))

(defun draw-bsp (tree &aux (i 15))
  (map-tree (lambda (area)
              (with-area (area)
                (let ((color (random-color 0.5)))
                  (blt:draw-box (incf i) x y w h ""
                                :border :light
                                :background-color color
                                :border-color color))))
            tree))
