(in-package :rl)

;;;; Parameters ---------------------------------------------------------------
(defvar *running* t)

(defparameter *screen-width* 60)
(defparameter *screen-height* 40)
(defparameter *cell-size* 16)

(defparameter *map-width* *screen-width*)
(defparameter *map-height* (- *screen-height* 5))

(defparameter *max-monsters-per-room* 3)

(defconstant +color-light-wall+ (blt:hsva 1.0 0.0 0.7))
(defconstant +color-light-ground+ (blt:hsva 1.0 0.0 0.5))
(defconstant +color-dark-wall+ (blt:hsva 1.0 0.0 0.4))
(defconstant +color-dark-ground+ (blt:hsva 1.0 0.0 0.3))


(defparameter *layer-bg* 0)
(defparameter *layer-mobs* 1)
(defparameter *layer-fov* 9)
(defparameter *layer-mouse* 10)


(defvar *mouse-x* 0)
(defvar *mouse-y* 0)

(defvar *enable-tiles* nil)
(defvar *show-mouse?* t)

(defparameter *assets-directory* "./assets/")


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
  ((glyph :initform #\? :type character)
   (seen :initform nil :type boolean)))

(defclass* ground (tile)
  ())

(defclass* wall (tile)
  ())

(defun make-wall ()
  (make-instance 'wall :glyph #\#))

(defun make-ground ()
  (make-instance 'ground :glyph #\.))


(defun tile-blocks-movement-p (tile)
  (etypecase tile
    (wall t)
    (ground nil)))

(defun tile-blocks-sight-p (tile)
  (etypecase tile
    (wall t)
    (ground nil)))


;;;; Map ----------------------------------------------------------------------
(defvar *map* nil)
(defvar *bsp* nil)
(defvar *rooms* nil)


(defun carve-tile (map x y)
  (setf (aref map x y) (make-ground)))

(defun make-blank-map ()
  (let ((map (make-array (list *map-width* *map-height*))))
    (dorange ((x 0 *map-width*)
              (y 0 *map-height*))
      (setf (aref map x y) (make-wall)))
    map))

(defun make-map ()
  (let ((map (make-blank-map))
        (bsp (rl.bsp::make-partitioning *map-width* *map-height*)))
    (setf *bsp* bsp)
    (rl.bsp::carve-partitioning (curry #'carve-tile map) bsp)
    (setf *rooms* (coerce (rl.bsp::flatten-partitioning *bsp*) 'vector))
    map))


(defun map-ref? (x y)
  (when (and (in-range-p 0 x *map-width*)
             (in-range-p 0 y *map-height*))
    (aref *map* x y)))

(defun find-wall-glyph (x y)
  (flet ((wallp (neighbor)
           (typep neighbor 'wall)))
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
           (when (typep tile 'wall)
             (setf (tile-glyph tile)
                   (find-wall-glyph x y)))))


(defun random-room ()
  (random-elt *rooms*))


;;;; Game Objects -------------------------------------------------------------
(defclass* object ()
  ((x :type fixnum)
   (y :type fixnum)
   (name :type string)
   (blocking :type boolean :initform t)
   (glyph :type character :initform #\?)
   (color :initform (blt:hsva 1.0 0.0 1.0))
   (layer :initform 0)))

(define-with-macro object x y glyph color layer)

(defun make-object (x y &rest key-slots)
  (apply #'make-instance 'object :x x :y y key-slots))

(defun make-mob (x y &rest key-slots)
  (apply #'make-object x y :layer *layer-mobs* key-slots))


(defun blockedp (x y)
  (or (tile-blocks-movement-p (map-ref? x y))
      (some (lambda (o)
              (and (= x (object-x o))
                   (= y (object-y o))))
            *objects*)))

(defun move-object (object dx dy)
  (with-object (object)
    (let ((tx (+ x dx))
          (ty (+ y dy)))
      (unless (or (not (in-range-p 0 tx *map-width*))
                  (not (in-range-p 0 ty *map-height*))
                  (blockedp tx ty))
        (setf x tx)
        (setf y ty))))
  (values))

(defun draw-object (object)
  (with-object (object)
    (when (visiblep x y)
      (setf (blt:layer) layer
            (blt:font) "tile"
            (blt:composition) t
            (blt:color) (blt:hsva 0 0 0)
            (blt:cell-char x y) #\full_block
            (blt:color) color
            (blt:cell-char x y) glyph
            (blt:composition) nil)))
  (values))

(defun clear-object (object)
  (with-object (object)
    (setf (blt:layer) layer
          (blt:cell-char x y) #\space))
  (values))


(defvar *player* nil)
(defvar *objects* nil)


(defun place-player-initial ()
  (setf (values (object-x *player*)
                (object-y *player*))
        (rl.bsp::random-room-point (random-room))))

(defun move-player (dx dy)
  (move-object *player* dx dy)
  (recompute-fov))


;;;; Config -------------------------------------------------------------------
(defun asset-path (filename)
  (concatenate 'string *assets-directory* filename))


(defun config-fonts ()
  (blt:set "font: ~A, size=~Dx~:*~D;"
           (asset-path "ProggySquare/ProggySquare.ttf")
           *cell-size*)
  (blt:set "tile font: ~A, size=16x16, resize=~Dx~:*~D, codepage=437;"
           (asset-path "df.png")
           *cell-size*)
  (blt:set "text font: ~A, size=~Dx~D;"
           (asset-path "UbuntuMono/UbuntuMono-R.ttf")
           *cell-size*
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
(defparameter *monster-selection*
  (make-weightlist '((60 . goblin)
                     (30 . orc)
                     (10 . troll))))

(defun make-goblin (x y)
  (make-mob x y :glyph #\g :color (blt:rgba 0 255 0) :name "A goblin"))

(defun make-orc (x y)
  (make-mob x y :glyph #\o :color (blt:rgba 0 255 0) :name "An orc"))

(defun make-troll (x y)
  (make-mob x y :glyph #\T :color (blt:rgba 0 200 0) :name "A troll"))

(defun random-monster (x y)
  (ecase (weightlist-random *monster-selection*)
    (goblin (make-goblin x y))
    (orc (make-orc x y))
    (troll (make-troll x y))))

(defun populate-room (room)
  (dorepeat (random (1+ *max-monsters-per-room*))
    (multiple-value-bind (x y)
        (rl.bsp::random-room-point room)
      (unless (blockedp x y)
        (push (random-monster x y) *objects*)))))

(defun populate-rooms ()
  (map nil #'populate-room *rooms*))


(defun make-player ()
  (make-mob (truncate *screen-width* 2)
            (truncate *screen-height* 2)
            :name "You"
            :glyph #\@
            :layer *layer-mobs*))

(defun generate-player ()
  (-<> (make-player)
    (setf *player* <>)
    (push <> *objects*)))

(defun generate-mobs ()
  (populate-rooms))


(defun generate-map ()
  (setf *map* (make-map)
        *fov-map* (make-fov-map)))


(defun initialize ()
  (place-player-initial)
  (rebuild-map-glyphs)
  (recompute-fov))


(defun generate ()
  (setf *objects* nil)
  (generate-map)
  (generate-player)
  (generate-mobs)
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
  (and (in-bounds-p x y)
       (tile-blocks-sight-p (aref *map* x y))))

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
  (labels ((tile-color (tile visible)
             (etypecase tile
               (wall (if visible
                       +color-light-wall+
                       +color-dark-wall+))
               (ground (if visible
                         +color-light-ground+
                         +color-dark-ground+))))
           (draw-tile (tile x y visible)
             (setf (blt:color) (tile-color tile visible)
                   (blt:cell-char x y) (tile-glyph tile))))
    (iterate (for (tile x y) :in-array *map*)
             (for visible = (aref *fov-map* x y))
             (for seen = (tile-seen tile))
             (when (and visible (not seen))
               (setf (tile-seen tile) t))
             (when (or visible seen)
               (draw-tile tile x y visible)))))

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

(defun draw-status ()
  (setf (blt:color) (blt:rgba 1.0 1.0 1.0)
        (blt:font) "text")
  (blt:print 0 (- *screen-height* 4)
             (format nil "F1: Refresh Config~%~
                          F2: Toggle Tiles~%~
                          F3: Toggle Mouse~%~
                          F4: Rebuild World"))
  (blt:print 25 (- *screen-height* 4)
             (format nil "F5: Reveal Map~3%~
                          ESC: Quit")))

(defun draw ()
  (blt:clear)
  (draw-map)
  (draw-objects)
  (draw-mouse)
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
  (multiple-value-bind (x y) (mouse-coords)
    (setf (aref *map* x y)
          (if (typep (aref *map* x y) 'wall)
            (make-ground)
            (make-wall))))
  (recompute-fov)
  (rebuild-map-glyphs))

(defun warp-player ()
  (multiple-value-bind (x y) (mouse-coords)
    (setf (object-x *player*) x
          (object-y *player*) y))
  (recompute-fov))


(defun reveal-map ()
  (do-array (tile *map*)
    (setf (tile-seen tile) t)))


(defun read-event ()
  (if (blt:has-input-p)
    (blt:key-case (blt:read)
      (:f1 :refresh-config)
      (:f2 :flip-tiles)
      (:f3 :flip-mouse)
      (:f4 :regenerate-world)
      (:f5 :reveal-map)

      (:up :move-up)
      (:down :move-down)
      (:left :move-left)
      (:right :move-right)

      (:numpad-1 :move-down-left)
      (:numpad-2 :move-down)
      (:numpad-3 :move-down-right)
      (:numpad-4 :move-left)
      (:numpad-6 :move-right)
      (:numpad-7 :move-up-left)
      (:numpad-8 :move-up)
      (:numpad-9 :move-up-right)

      (:mouse-move :mouse-move)
      (:mouse-left :toggle-wall)
      (:mouse-right :warp-player)

      (:escape :quit)
      (:close :quit))
    :done))

(defun handle-event (event)
  "Handle the given event.

  Returns two values: whether the screen needs to be redrawn, and whether the
  game time should be ticked.

  "
  (ecase event
    (:refresh-config (config) (values t nil))
    (:flip-tiles (notf *enable-tiles*) (config) (values t))
    (:flip-mouse (notf *show-mouse?*) (update-mouse-location) (values t nil))
    (:mouse-move (update-mouse-location) (values t nil))
    (:reveal-map (reveal-map) (values t nil))
    (:toggle-wall (toggle-wall) (values t nil))
    (:regenerate-world (generate) (values t nil))
    (:warp-player (warp-player) (values t nil))

    (:move-up    (move-player  0 -1) (values t t))
    (:move-down  (move-player  0  1) (values t t))
    (:move-left  (move-player -1  0) (values t t))
    (:move-right (move-player  1  0) (values t t))
    (:move-up-left    (move-player -1 -1) (values t t))
    (:move-up-right   (move-player  1 -1) (values t t))
    (:move-down-left  (move-player -1  1) (values t t))
    (:move-down-right (move-player  1  1) (values t t))

    (:quit (setf *running* (values nil nil)))))

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
