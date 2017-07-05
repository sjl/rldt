(in-package :rl)

;;;; Parameters ---------------------------------------------------------------
(defvar *running* t)

(defparameter *screen-width* 80)
(defparameter *screen-height* 60)
(defparameter *cell-size* 16)

(defparameter *map-width* *screen-width*)
(defparameter *map-height* (- *screen-height* 5))

(defconstant +color-dark-wall+ (blt:hsva 1.0 0.0 0.7))
(defconstant +color-dark-ground+ (blt:hsva 1.0 0.0 0.5))

(defparameter *layer-bg* 0)
(defparameter *layer-mobs* 1)
(defparameter *layer-fov* 9)
(defparameter *layer-mouse* 10)

(defvar *mouse-x* 0)
(defvar *mouse-y* 0)

(defvar *enable-tiles* nil)
(defvar *show-mouse?* t)

(defparameter *assets-directory* "./assets/")


;;;; Utils --------------------------------------------------------------------
(defun asset-path (filename)
  (concatenate 'string *assets-directory* filename))

(defmacro sortf (place-1 place-2 &key (key ''<))
  (with-gensyms (value-1 value-2)
    `(let ((,value-1 ,place-1)
           (,value-2 ,place-2))
       (when (funcall ,key ,value-2 ,value-1)
         (rotatef ,place-1 ,place-2)))))


;;;; Lines --------------------------------------------------------------------
(defun-inline distance (x1 y1 x2 y2)
  (max (abs (- x1 x2))
       (abs (- y1 y2))))

(defun line (x1 y1 x2 y2)
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
  (blocks-movement blocks-sight glyph))


(defun make-tile (&key blocks-movement (blocks-sight blocks-movement))
  (make-instance 'tile
    :blocks-movement blocks-movement
    :blocks-sight blocks-sight
    :glyph #\.))

(defun make-wall ()
  (make-tile :blocks-movement t :blocks-sight t))


;;;; Map ----------------------------------------------------------------------
(defvar *map* nil)
(defvar *bsp* nil)


(defun carve-tile (map x y)
  (let ((tile (aref map x y)))
    (setf (tile-blocks-sight tile) nil
          (tile-blocks-movement tile) nil)))

(defun make-blank-map ()
  (let ((map (make-array (list *map-width* *map-height*))))
    (iterate
      (for-nested ((x :from 0 :below *map-width*)
                   (y :from 0 :below *map-height*)))
      (setf (aref map x y) (make-wall)))
    map))

(defun make-map ()
  (let ((map (make-blank-map))
        (bsp (rl.bsp::make-partitioning *map-width* *map-height*)))
    (setf *bsp* bsp)
    (rl.bsp::carve-partitioning (curry #'carve-tile map) bsp)
    map))


(defun map-blocks-movement-p (x y)
  (tile-blocks-movement (aref *map* x y)))

(defun map-blocks-sight-p (x y)
  (tile-blocks-sight (aref *map* x y)))


(defun map-ref? (x y)
  (when (and (in-range-p 0 x *map-width*)
             (in-range-p 0 y *map-height*))
    (aref *map* x y)))

(defun find-wall-glyph (x y)
  (flet ((wallp (neighbor)
           (and neighbor (tile-blocks-movement neighbor))))
    (let ((u (wallp (map-ref? x (1- y))))
          (d (wallp (map-ref? x (1+ y))))
          (l (wallp (map-ref? (1- x) y)))
          (r (wallp (map-ref? (1+ x) y))))
      (cond
        ((and u d l r) #\box_drawings_double_vertical_and_horizontal)
        ((and   d l r) #\box_drawings_double_down_and_horizontal)
        ((and u   l r) #\box_drawings_double_up_and_horizontal)
        ((and u d   r) #\box_drawings_double_vertical_and_right)
        ((and u d l  ) #\box_drawings_double_vertical_and_left)
        ((and u d    ) #\box_drawings_double_vertical)
        ((and     l r) #\box_drawings_double_horizontal)
        ((and u   l  ) #\box_drawings_double_up_and_left)
        ((and u     r) #\box_drawings_double_up_and_right)
        ((and   d l  ) #\box_drawings_double_down_and_left)
        ((and   d   r) #\box_drawings_double_down_and_right)
        ((and u      ) #\box_drawings_double_vertical)
        ((and   d    ) #\box_drawings_double_vertical)
        ((and     l  ) #\box_drawings_double_horizontal)
        ((and       r) #\box_drawings_double_horizontal)
        (t             #\identical_to)))))

(defun rebuild-map-glyphs ()
  (iterate (for (tile x y) :in-array *map*)
           (setf (tile-glyph tile)
                 (if (and (tile-blocks-sight tile)
                          (tile-blocks-movement tile))
                   (find-wall-glyph x y)
                   #\.))))


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
    (let ((tx (+ x dx))
          (ty (+ y dy)))
      (unless (or (not (in-range-p 0 tx *map-width*))
                  (not (in-range-p 0 ty *map-height*))
                  (map-blocks-movement-p tx ty))
        (setf x tx)
        (setf y ty))))
  (values))

(defun draw-object (object)
  (with-object (object)
    (setf (blt:layer) layer
          (blt:color) color
          (blt:font) "tile"
          (blt:cell-char x y) glyph))
  (values))

(defun clear-object (object)
  (with-object (object)
    (setf (blt:layer) layer
          (blt:cell-char x y) #\space))
  (values))


(defvar *player* nil)
(defvar *fungus* nil)
(defvar *goblin* nil)
(defvar *objects* nil)


(defun place-player-initial ()
  (setf (values (object-x *player*)
                (object-y *player*))
        (rl.bsp::random-room-point (random-elt (rl.bsp::flatten-partitioning *bsp*)))))

;;;; Config -------------------------------------------------------------------
(defun config-fonts ()
  (blt:set "font: ~A, size=~Dx~:*~D;"
           (asset-path "ProggySquare/ProggySquare.ttf")
           *cell-size*)
  (blt:set "tile font: ~A, size=16x16, resize=~Dx~:*~D, codepage=437;"
           (asset-path "df.png")
           *cell-size*)
  (when *enable-tiles*
    (blt:set "tile 0x0000: ~A, size=16x16, resize=~Dx~:*~D, align=dead-center, codepage=~A;"
             (asset-path "tiles.png")
             *cell-size*
             (asset-path "codepage.txt"))))

(defun config ()
  (blt:set (format nil "window.size = ~Dx~D" *screen-width* *screen-height*))
  (blt:set "window.title = /r/roguelikedev")
  (blt:set "window.cellsize = ~Dx~:*~D" *cell-size*)
  (blt:set "output.vsync = true")
  (blt:set "input.filter = keyboard, mouse")
  (config-fonts)
  (setf *running* t))


;;;; Generation ---------------------------------------------------------------
(defun generate-player ()
  (setf *player*
        (make-object (truncate *screen-width* 2)
                     (truncate *screen-height* 2)
                     :glyph #\@
                     :layer *layer-mobs*)))

(defun generate-mobs ()
  (setf *fungus*
        (make-object 4 4
                     :glyph #\F
                     :color (blt:hsva 0.3 0.8 1.0)
                     :layer *layer-mobs*)
        *goblin*
        (make-object 2 2
                     :glyph #\g
                     :layer *layer-mobs*)))

(defun generate-objects ()
  (generate-player)
  (generate-mobs)
  (setf *objects* (list *player* *fungus* *goblin*)))


(defun generate-map ()
  (setf *map* (make-map)
        *fov-map* (make-fov-map)))


(defun initialize ()
  (place-player-initial)
  (rebuild-map-glyphs)
  (recompute-fov))


(defun generate ()
  (generate-objects)
  (generate-map)
  (initialize)
  (values))


;;;; Vision -------------------------------------------------------------------
(defun make-fov-map ()
  (make-array (array-dimensions *map*) :element-type 'boolean :initial-element nil))

(defvar *fov-map* nil)

(defun clear-fov-map ()
  (fill-multidimensional-array *fov-map* nil))


(defun-inline in-bounds-p (x y)
  (and (in-range-p 0 x *map-width*)
       (in-range-p 0 y *map-height*)))

(defun opaquep (x y)
  (if (in-bounds-p x y)
    (map-blocks-sight-p x y)
    nil))

(defun mark-visible (x y)
  (when (in-bounds-p x y)
    (setf (aref *fov-map* x y) t)))

(defun visiblep (x y)
  (aref *fov-map* x y))

(defun recompute-fov ()
  (clear-fov-map)
  (compute-fov (object-x *player*) (object-y *player*) 10
               #'opaquep #'mark-visible))


;;;; Rendering ----------------------------------------------------------------
(defun draw-objects ()
  (map nil #'draw-object *objects*))

(defun draw-map ()
  (setf (blt:layer) *layer-bg*
        (blt:font) "tile")
  (iterate (for (tile x y) :in-array *map*)
           (for wall? = (tile-blocks-sight tile))
           (setf (blt:color) (if wall? +color-dark-wall+ +color-dark-ground+)
                 (blt:cell-char x y) (tile-glyph tile))))

(defun draw-mouse ()
  (when *show-mouse?*
    (setf (blt:layer) *layer-mouse*
          (blt:color) (blt:hsva 1.0 0.0 1.0 0.5))
    (iterate
      (for (x . y) :in-vector (line (object-x *player*) (object-y *player*)
                                    *mouse-x* *mouse-y*))
      (setf (blt:cell-char x y) #\full_block))
    (setf (blt:color) (blt:hsva 1.0 0.0 1.0 0.8)
          (blt:cell-char *mouse-x* *mouse-y*) #\full_block)))

(defun draw-fov ()
  (setf (blt:layer) *layer-fov*
        (blt:color) (blt:hsva 0.3 0.3 1.0 0.3))
  (iterate
    (for (visible? x y) :in-array *fov-map*)
    (when visible?
      (setf (blt:cell-char x y) #\full_block))))

(defun draw-status ()
  (setf (blt:color) (blt:rgba 1.0 1.0 1.0)
        (blt:font) nil)
  (blt:print 0 (1- *screen-height*) "Hello."))

(defun draw ()
  (blt:clear)
  (draw-map)
  (draw-objects)
  (draw-mouse)
  (draw-fov)
  (draw-status)
  (blt:refresh))


;;;; Input --------------------------------------------------------------------
(defun update-mouse-location ()
  (setf (values *mouse-x* *mouse-y*) (blt:mouse)))


(defun mouse-coords ()
  (values *mouse-x* *mouse-y*))

(defun mouse-tile ()
  (multiple-value-call #'aref *map* (mouse-coords)))

(defun toggle-wall ()
  (let ((tile (mouse-tile)))
    (notf (tile-blocks-movement tile))
    (notf (tile-blocks-sight tile))
    (recompute-fov)
    (rebuild-map-glyphs)))

(defun warp-player ()
  (multiple-value-bind (x y) (mouse-coords)
    (setf (object-x *player*) x
          (object-y *player*) y))
  (recompute-fov))


(defun read-event ()
  (if (blt:has-input-p)
    (blt:key-case (blt:read)
      (:f1 :refresh-config)
      (:f2 :flip-tiles)
      (:f3 :flip-mouse)
      (:f4 :regenerate-world)

      (:up :move-up)
      (:down :move-down)
      (:left :move-left)
      (:right :move-right)

      (:mouse-move :mouse-move)
      (:mouse-left :toggle-wall)
      (:mouse-right :warp-player)

      (:escape :quit)
      (:close :quit))
    :done))

(defun handle-event (event)
  (ecase event
    (:refresh-config (config) t)
    (:flip-tiles (notf *enable-tiles*) (config) t)
    (:flip-mouse (notf *show-mouse?*) (update-mouse-location) t)
    (:mouse-move (update-mouse-location) t)
    (:toggle-wall (toggle-wall) t)
    (:regenerate-world (generate) t)
    (:move-up (move-object *player* 0 -1) (recompute-fov) t)
    (:move-down (move-object *player* 0 1) (recompute-fov) t)
    (:move-left (move-object *player* -1 0) (recompute-fov) t)
    (:move-right (move-object *player* 1 0) (recompute-fov) t)
    (:warp-player (warp-player) t)
    (:quit (setf *running* nil))))

(defun handle-events ()
  (iterate
    (for event = (read-event))
    (until (eql event :done))
    (when event
      (oring (handle-event event)))))


;;;; Main Loop ----------------------------------------------------------------
(defun run ()
  (blt:with-terminal
    (config)
    (generate)
    (blt:clear)
    (iterate
      (while *running*)
      (if skip-draw
        (sleep 1/60)
        (draw))
      (for had-events = (handle-events))
      (for skip-draw = (not had-events)))))


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
