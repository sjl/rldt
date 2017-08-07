(in-package :rl.panels)

;;;; Data ---------------------------------------------------------------------
(defparameter *panel-initial-layer* 0)
(defvar *panels* (make-array 10
                   :adjustable t
                   :fill-pointer 0
                   :initial-element nil))
(defvar *screen-width* 60)
(defvar *screen-height* 40)


;;;; Class --------------------------------------------------------------------
(defclass* panel ()
  (x
   y
   width
   height
   layer
   layers
   border
   border-color
   background-color
   compute-geometry
   draw-panel
   name))

(defun make-panel (name compute-geometry draw-panel
                   &key border border-color background-color layers)
  (multiple-value-bind (x y width height)
      (funcall compute-geometry *screen-width* *screen-height*)
    (make-instance 'panel
      :x x :y y :width width :height height
      :compute-geometry compute-geometry
      :draw-panel draw-panel
      :layers layers
      :border border
      :border-color border-color
      :background-color background-color
      :name name)))

(defmethod print-object ((p panel) stream)
  (print-unreadable-object (p stream :type t :identity t)
    (princ (panel-name p) stream)))


;;;; Geometry -----------------------------------------------------------------
(defun fixed (x y width height)
  (lambda (w h)
    (declare (ignore w h))
    (values x y width height)))

(defun stretch ()
  (lambda (width height)
    (values 0 0 width height)))

(defun stretch-horizontally (fixed-height &optional (vertical-position :top))
  (lambda (width height)
    (values 0
            (etypecase vertical-position
              ((eql :top) 0)
              ((eql :bottom) (- height fixed-height))
              (integer vertical-position))
            width fixed-height)))

(defun stretch-vertically (fixed-width &optional (horizontal-position :left))
  (lambda (width height)
    (values (etypecase horizontal-position
              ((eql :left) 0)
              ((eql :right) (- width fixed-width))
              (integer horizontal-position))
            0
            fixed-width height)))


;;;; Stack Management ---------------------------------------------------------
(defun vector-pop-harder (vector)
  (prog1
      (aref vector (decf (fill-pointer vector)))
    (setf (aref vector (fill-pointer vector)) nil)))

(defun push-panel (panel)
  (setf (panel-layer panel)
        (if-found (previous (vector-last *panels*))
          (+ (panel-layer previous) (panel-layers previous))
          *panel-initial-layer*))
  (vector-push-extend panel *panels*))

(defun pop-panel (&optional intended-panel)
  (let ((popped (vector-pop-harder *panels*)))
    (when intended-panel
      (assert (eq intended-panel popped) ()
        "Popped a panel other than the one intended to be popped.")))
  (values))

(defmacro with-panel ((panel-name panel-var compute-geometry draw-panel &key
                                  border (border-color (blt:white))
                                  background-color (layers 2))
                      &body body)
  `(let ((,panel-var (make-panel ,panel-name ,compute-geometry ,draw-panel
                                 :layers ,layers
                                 :border ,border
                                 :border-color ,border-color
                                 :background-color ,background-color)))
     (push-panel ,panel-var)
     (unwind-protect (progn ,@body)
       (pop-panel ,panel-var))))

(defmacro with-panels (panels &body body)
  (if (null panels)
    `(progn ,@body)
    `(with-panel ,(first panels)
       (with-panels ,(rest panels)
         ,@body))))


;;;; Event Handling -----------------------------------------------------------
(defun recompute-panel-information (panel)
  (multiple-value-bind (x y width height)
      (funcall (panel-compute-geometry panel) *screen-width* *screen-height*)
    (setf (panel-x panel) x
          (panel-y panel) y
          (panel-width panel) width
          (panel-height panel) height)))

(defun recompute-panels-information ()
  (map nil #'recompute-panel-information *panels*))


(defun handle-event-for-panels (event)
  (blt:key-case event
    (:resize
     (setf *screen-width* (blt:width)
           *screen-height* (blt:height))
     (recompute-panels-information)
     t)))


;;;; Initialization -----------------------------------------------------------
(defun initialize-panels ()
  (setf *screen-width* (blt:width)
        *screen-height* (blt:height)))


;;;; Printing -----------------------------------------------------------------
(defun compute-coordinates (panel x y)
  (values (+ x (panel-x panel))
          (+ y (panel-y panel))))


(defun print (panel x y string
              &key width height (halign :default) (valign :default))
  (multiple-value-call #'blt:print
    (compute-coordinates panel x y)
    string
    :width width :height height
    :halign halign :valign valign))


(defun (setf cell-code) (code-point panel x y)
  (multiple-value-bind (x y) (compute-coordinates panel x y)
    (setf (blt:cell-code x y) code-point)))

(defun (setf cell-char) (character panel x y)
  (multiple-value-bind (x y) (compute-coordinates panel x y)
    (setf (blt:cell-char x y) character)))


;;;; Drawing ------------------------------------------------------------------
(defun draw-panel-box (panel)
  (setf (blt:font) "simple")
  (blt:draw-box (panel-x panel) (panel-y panel)
                (panel-width panel) (panel-height panel)
                :border (panel-border panel)
                :border-color (panel-border-color panel)
                :background-color (panel-background-color panel)))

(defun draw-panel (panel)
  (setf (blt:layer) (panel-layer panel))
  (draw-panel-box panel)
  (incf (blt:layer))
  (funcall (panel-draw-panel panel) panel))

(defun draw-panels ()
  (map nil #'draw-panel *panels*))

