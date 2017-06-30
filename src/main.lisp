(in-package :rl)

;;;; Parameters ---------------------------------------------------------------
(defvar *running* t)

(defparameter *screen-width* 40)
(defparameter *screen-height* 40)

(defparameter *map-width* 40)
(defparameter *map-height* 35)

(defconstant +color-dark-wall+ (blt:hsva 1.0 0.0 0.7))
(defconstant +color-dark-ground+ (blt:hsva 1.0 0.0 0.5))

(defparameter *layer-bg* 0)
(defparameter *layer-mobs* 1)
(defparameter *layer-mouse* 10)

(defparameter *mouse-x* 0)
(defparameter *mouse-y* 0)

(defparameter *enable-tiles* nil)
(defparameter *show-mouse?* t)

(defparameter *assets-directory* "./assets/")


;;;; Utils --------------------------------------------------------------------
(defun print-tile (x y glyph)
  ;; todo: replace with (blt:font) for the next version of blt
  (blt:print x y (format nil "[font=tile]~A" glyph)))

(defun asset-path (filename)
  (concatenate 'string *assets-directory* filename))


;;;; Lines --------------------------------------------------------------------
(defun-inline distance (x1 y1 x2 y2)
  (max (abs (- x1 x2))
       (abs (- y1 y2))))

(defun line-lerp% (x1 y1 x2 y2)
  (declare (type fixnum x1 y1 x2 y2)
           (optimize (speed 3)))
  (if (and (= x1 x2) (= y1 y2))
    (make-array 1 :initial-element (cons x1 y1) :fill-pointer 1)
    (iterate
      (declare (type fixnum distance points _)
               (type single-float n x1f x2f y1f y2f))
      (with distance = (distance x1 y1 x2 y2))
      (with points = (1+ distance))
      (with result = (make-array points :fill-pointer 0))
      (with x1f = (float x1))
      (with x2f = (float x2))
      (with y1f = (float y1))
      (with y2f = (float y2))
      (for _ :from 0 :below points)
      (for n :from 0.0 :by (/ 1.0 distance))
      (vector-push (cons (round (lerp x1f x2f n))
                         (round (lerp y1f y2f n)))
                   result)
      (finally (return result)))))

(defun line-lerp (x1 y1 x2 y2)
  (if (and (= x1 x2) (= y1 y2))
    (make-array 1 :initial-element (cons x1 y1) :fill-pointer 1)
    (iterate
      (with distance = (distance x1 y1 x2 y2))
      (with points = (1+ distance))
      (with result = (make-array points :fill-pointer 0))
      (repeat points)
      (for n :from 0.0 :by (/ 1.0 distance))
      (vector-push (cons (round (lerp x1 x2 n))
                         (round (lerp y1 y2 n)))
                   result)
      (finally (return result)))))


;;;; Tiles --------------------------------------------------------------------
(defclass* tile ()
  (blocks-movement blocks-sight))


(defun make-tile (&key blocks-movement (blocks-sight blocks-movement))
  (make-instance 'tile
    :blocks-movement blocks-movement
    :blocks-sight blocks-sight))

(defun make-map ()
  (let ((map (make-array (list *map-width* *map-height*))))
    (iterate
      (for-nested ((x :from 0 :below *map-width*)
                   (y :from 0 :below *map-height*)))
      (setf (aref map x y) (make-tile)))
    (setf (tile-blocks-movement (aref map 5 5)) t
          (tile-blocks-sight (aref map 5 5)) t
          (tile-blocks-movement (aref map 15 5)) t
          (tile-blocks-sight (aref map 15 5)) t)
    map))


(defparameter *map* (make-map))


(defun map-blocks-movement-p (x y)
  (tile-blocks-movement (aref *map* x y)))


;;;; Game Objects -------------------------------------------------------------
(defclass* object ()
  ((x :type fixnum)
   (y :type fixnum)
   (glyph :type character :initform #\?)
   (color :initform (blt:hsva 1.0 0.0 1.0))
   (layer :initform 0)))

(define-with-macro object x y glyph color layer)

(defun make-object (x y &rest key-slots)
  (apply #'make-instance 'object :x x :y y key-slots))


(defun move-object (object dx dy)
  (with-object (object)
    (unless (map-blocks-movement-p (+ x dx) (+ y dy))
      (incf x dx)
      (incf y dy)))
  (values))

(defun draw-object (object)
  (with-object (object)
    (setf (blt:layer) layer
          (blt:color) color)
    (print-tile x y glyph))
  (values))

(defun clear-object (object)
  (with-object (object)
    (setf (blt:layer) layer
          (blt:cell-char x y) #\space))
  (values))


(defparameter *player*
  (make-object (truncate *screen-width* 2)
               (truncate *screen-height* 2)
               :glyph #\@
               :layer *layer-mobs*))

(defparameter *fungus*
  (make-object 4 4
               :glyph #\F
               :color (blt:hsva 0.3 0.8 1.0)
               :layer *layer-mobs*))

(defparameter *goblin*
  (make-object 2 2
               :glyph #\g
               :layer *layer-mobs*))

(defparameter *objects* (list *player* *fungus* *goblin*))


;;;; Config -------------------------------------------------------------------
(defun config-fonts ()
  (blt:set "font: ~A, size=16x16, align=dead-center;"
           (asset-path "ProggySquare/ProggySquare.ttf"))
  (blt:set "tile font: ~A, size=16x16, align=dead-center;"
           (asset-path "ProggySquare/ProggySquare.ttf"))
  (when *enable-tiles*
    (blt:set "tile 0x0000: ~A, size=16x16, resize=16x16, align=dead-center, codepage=~A;"
             (asset-path "tiles.png")
             (asset-path "codepage.txt"))))

(defun config ()
  (blt:set (format nil "window.size = ~Dx~D" *screen-width* *screen-height*))
  (blt:set "window.title = /r/roguelikedev")
  (blt:set "window.cellsize = 16x16")
  (blt:set "output.vsync = true")
  (blt:set "input.filter = keyboard, mouse")
  (config-fonts)
  (setf *running* t))


;;;; Rendering ----------------------------------------------------------------
(defun draw-objects ()
  (map nil #'draw-object *objects*))

(defun clear-objects ()
  (map nil #'clear-object *objects*))


(defun draw-map ()
  (setf (blt:layer) *layer-bg*)
  (iterate (for (tile x y) :in-array *map*)
           (for wall? = (tile-blocks-sight tile))
           (setf (blt:color) (if wall? +color-dark-wall+ +color-dark-ground+))
           (print-tile x y (if wall? #\# #\.))))


(defun draw-mouse ()
  (when *show-mouse?*
    (setf (blt:layer) *layer-mouse*
          (blt:color) (blt:hsva 1.0 0.0 1.0 0.5))
    (iterate (for (x . y) :in-vector (line-bres 0 0 *mouse-x* *mouse-y*))
             (setf (blt:cell-char x y) #\full_block))
    (setf (blt:color) (blt:hsva 1.0 0.0 1.0 0.8)
          (blt:cell-char *mouse-x* *mouse-y*) #\full_block)))

(defun clear-mouse ()
  (when *show-mouse?*
    (blt:clear-layer *layer-mouse*)))


(defun draw ()
  (draw-map)
  (draw-objects)
  (draw-mouse)
  (blt:refresh)
  (clear-objects)
  (clear-mouse))


;;;; Input --------------------------------------------------------------------
(defun update-mouse-location ()
  (when *show-mouse?*
    (setf (values *mouse-x* *mouse-y*) (blt:mouse))))

(defun read-event ()
  (if (blt:has-input-p)
    (blt:key-case (blt:read)
      (:f1 :refresh-config)
      (:f2 :flip-tiles)
      (:f3 :flip-mouse)

      (:up :move-up)
      (:down :move-down)
      (:left :move-left)
      (:right :move-right)

      (:mouse-move :mouse-move)

      (:escape :quit)
      (:close :quit))
    :done))

(defun handle-event (event)
  (ecase event
    (:refresh-config (config))
    (:flip-tiles (notf *enable-tiles*) (config))
    (:flip-mouse (notf *show-mouse?*) (update-mouse-location))
    (:mouse-move (update-mouse-location))
    (:move-up (move-object *player* 0 -1))
    (:move-down (move-object *player* 0 1))
    (:move-left (move-object *player* -1 0))
    (:move-right (move-object *player* 1 0))
    (:quit (setf *running* nil))))

(defun handle-events ()
  (iterate
    (for event = (read-event))
    (until (eql event :done))
    (when event
      (handle-event event))))


;;;; Main Loop ----------------------------------------------------------------
(defun run ()
  (blt:with-terminal
    (config)
    (iterate (while *running*)
             (draw)
             (handle-events))))


;;;; Entry --------------------------------------------------------------------
(defun unfuck-mac-path ()
  (sb-posix:chdir (let ((suffix "Contents/MacOS/rldt")
                        (path (uiop:native-namestring sb-ext:*runtime-pathname*)))
                    (subseq path 0 (- (length path) (length suffix)))))
  t)


(defun main ()
  (setf *random-state* (make-random-state t))
  (run)
  t)

(defun main-mac ()
  (unfuck-mac-path)
  (cffi:use-foreign-library blt:bearlibterminal)
  (setf *assets-directory* "./Contents/Resources/assets/")
  (main))
