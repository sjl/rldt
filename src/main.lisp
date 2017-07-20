(in-package :rl)

;;;; Parameters ---------------------------------------------------------------
(defvar *running* t)

(defparameter *screen-width* 64)
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
(defparameter *layer-mouse* 5)


(defvar *mouse-x* 0)
(defvar *mouse-y* 0)

(defvar *enable-tiles* nil)
(defvar *show-mouse?* t)

(defparameter *assets-directory* "./assets/")


;;;; Types --------------------------------------------------------------------
(deftype layer ()
  '(integer 0 10000))

(deftype coordinate ()
  '(integer 0 10000))


;;;; Coordinate Systems -------------------------------------------------------
(defun print (x y string &key width height (halign :default) (valign :default))
  (blt:print (* 2 x) (* 2 y) string
             :width (when width (* 2 width))
             :height (when height (* 2 height))
             :halign halign
             :valign valign))

(defun (setf cell-char) (new-value x y)
  (setf (blt:cell-char (* 2 x) (* 2 y)) new-value))


(defun-inline in-bounds-p (x y)
  (and (in-range-p 0 x *map-width*)
       (in-range-p 0 y *map-height*)))


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
  (when (in-bounds-p x y)
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


;;;; Aspects ------------------------------------------------------------------

;;; Renderable
(define-aspect renderable
  (glyph :type character :initform #\?)
  (color :initform (blt:hsva 0.0 0.0 1.0))
  (layer :type layer :initform *layer-mobs*))


;;; Flavor
(define-aspect flavor
  (name :type string :initform "Something"))


;;; Locations
(defvar *locations* nil)

(define-aspect loc
  (x :type coordinate)
  (y :type coordinate))

(defun initialize-locations ()
  (setf *locations* (make-array (list *map-width* *map-height*)
                      :initial-element nil
                      :element-type 'list)))

(defun loc-insert-entity (e)
  (push e (aref *locations* (loc/x e) (loc/y e))))

(defun loc-remove-entity (e)
  (zapf (aref *locations* (loc/x e) (loc/y e))
        (delete e %)))

(defun loc-move-entity (e new-x new-y)
  (loc-remove-entity e)
  (setf (loc/x e) new-x
        (loc/y e) new-y)
  (loc-insert-entity e))

(defun lref (x y)
  (aref *locations* x y))

(defmethod entity-created :after ((entity loc))
  (loc-insert-entity entity))

(defmethod entity-destroyed :after ((entity loc))
  (loc-remove-entity entity))


;;; Brains
(define-aspect brain)


;;;; Entities -----------------------------------------------------------------

;;;; Monsters
(define-entity monster (loc renderable flavor brain))

(defun create-goblin (x y)
  (create-entity 'monster
    :loc/x x
    :loc/y y
    :renderable/glyph #\g
    :renderable/color (blt:rgba 0 255 0)
    :flavor/name "A goblin"))

(defun create-orc (x y)
  (create-entity 'monster
    :loc/x x
    :loc/y y
    :renderable/glyph #\o
    :renderable/color (blt:rgba 0 200 80)
    :flavor/name "An orc"))

(defun create-troll (x y)
  (create-entity 'monster
    :loc/x x
    :loc/y y
    :renderable/glyph #\T
    :renderable/color (blt:rgba 0 180 0)
    :flavor/name "A troll"))


;;; Player
(define-entity player (loc renderable flavor))

(defun create-player ()
  (create-entity 'player
    :loc/x 0 ; loc will be set later, after map generation
    :loc/y 0
    :renderable/glyph #\@
    :flavor/name "You"))


;;;; Game Logic ---------------------------------------------------------------
(defvar *player* nil)


(defun object-at (x y)
  (when (in-bounds-p x y)
    (first (lref x y))))

(defun blockedp (x y)
  (or (tile-blocks-movement-p (map-ref? x y))
      (object-at x y)))

(defun move (entity x y)
  (if (or (not (in-bounds-p x y))
          (blockedp x y))
    nil
    (progn (loc-move-entity entity x y)
           t)))


(defun place-player-initial ()
  (multiple-value-call #'loc-move-entity *player*
    (rl.bsp::random-room-point (random-room))))


(defun player-move (x y)
  (prog1
      (move *player* x y)
    (recompute-fov)))

(defun player-attack (target)
  (pr 'attacking (flavor/name target))
  t)

(defun player-move-or-attack (dx dy)
  (let* ((tx (+ (loc/x *player*) dx))
         (ty (+ (loc/y *player*) dy))
         (target (object-at tx ty)))
    (if target
      (player-attack target)
      (player-move tx ty))))


;;;; Systems ------------------------------------------------------------------

;;; Rendering
(defun draw-entity (entity)
  (let ((x (loc/x entity))
        (y (loc/y entity)))
    (when (visiblep x y)
      (setf (blt:layer) (renderable/layer entity)
            (blt:font) "tile"
            (blt:composition) t
            (blt:color) (blt:hsva 0 0 0)
            (cell-char x y) #\full_block
            (blt:color) (renderable/color entity)
            (cell-char x y) (renderable/glyph entity)
            (blt:composition) nil)))
  (values))

(define-system render ((entity renderable loc))
  (draw-entity entity))


;;; AI
(defvar *turn* 0)

(define-system act ((entity brain))
  entity)

(defun tick ()
  (incf *turn*)
  (run-act))


;;;; Config -------------------------------------------------------------------
(defun asset-path (filename)
  (concatenate 'string *assets-directory* filename))

(defun config-fonts ()
  (blt:set "font: ~A, size=~Dx~:*~D, spacing=2x2;"
           (asset-path "ProggySquare/ProggySquare.ttf")
           *cell-size*)
  (blt:set "tile font: ~A, size=16x16, resize=~Dx~:*~D, codepage=437, spacing=2x2;"
           (asset-path "df.png")
           *cell-size*)
  (blt:set "text font: ~A, size=~Dx~D, spacing=1x2;"
           (asset-path "UbuntuMono/UbuntuMono-R.ttf")
           *cell-size*
           *cell-size*)
  (when *enable-tiles*
    (blt:set "tile 0x0000: ~A, size=16x16, resize=~Dx~:*~D, align=dead-center, codepage=~A, spacing=2x2;"
             (asset-path "tiles.png")
             *cell-size*
             (asset-path "codepage.txt"))))

(defun config ()
  (assert (evenp *cell-size*))
  (blt:set (format nil "window.size = ~Dx~D" (* 2 *screen-width*) (* 2 *screen-height*)))
  (blt:set "window.title = /r/roguelikedev")
  (blt:set "window.cellsize = ~Dx~:*~D" (floor *cell-size* 2))
  (blt:set "output.vsync = true")
  (blt:set "input.filter = keyboard, mouse")
  (config-fonts)
  (setf *running* t))


;;;; Generation ---------------------------------------------------------------
(defparameter *monster-selection*
  (make-weightlist '((60 . goblin)
                     (30 . orc)
                     (10 . troll))))


(defun create-random-monster (x y)
  (ecase (weightlist-random *monster-selection*)
    (goblin (create-goblin x y))
    (orc (create-orc x y))
    (troll (create-troll x y))))

(defun populate-room (room)
  (dorepeat (random (1+ *max-monsters-per-room*))
    (multiple-value-bind (x y)
        (rl.bsp::random-room-point room)
      (unless (blockedp x y)
        (create-random-monster x y)))))

(defun populate-rooms ()
  (map nil #'populate-room *rooms*))


(defun generate-player ()
  (setf *player* (create-player)))

(defun generate-mobs ()
  (populate-rooms))

(defun generate-map ()
  (setf *map* (make-map)
        *fov-map* (make-fov-map)))


(defun generate ()
  (clear-entities)
  (initialize-locations)
  (generate-map)
  (generate-player)
  (generate-mobs)
  (place-player-initial)
  (rebuild-map-glyphs)
  (recompute-fov)
  (values))


;;;; Vision -------------------------------------------------------------------
(defun make-fov-map ()
  (make-array (array-dimensions *map*) :element-type 'boolean :initial-element nil))

(defvar *fov-map* nil)

(defun clear-fov-map ()
  (fill-multidimensional-array *fov-map* nil))


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
  (compute-fov (loc/x *player*) (loc/y *player*) 10
               #'opaquep #'mark-visible))


;;;; Rendering ----------------------------------------------------------------
(defun tile-color (tile visible?)
  (etypecase tile
    (wall (if visible?
            +color-light-wall+
            +color-dark-wall+))
    (ground (if visible?
              +color-light-ground+
              +color-dark-ground+))))

(defun draw-map ()
  (setf (blt:layer) *layer-bg*
        (blt:font) "tile")
  (flet ((draw-tile (tile x y visible)
           (setf (blt:color) (tile-color tile visible)
                 (cell-char x y) (tile-glyph tile))))
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
      (for (x . y) :in-vector (line (loc/x *player*) (loc/y *player*)
                                    *mouse-x* *mouse-y*))
      (setf (cell-char x y) #\full_block))
    (setf (blt:color) (blt:hsva 1.0 0.0 1.0 0.8)
          (cell-char *mouse-x* *mouse-y*) #\full_block)))

(defun draw-status ()
  (setf (blt:color) (blt:rgba 1.0 1.0 1.0)
        (blt:font) "text")
  (print (- *screen-width* 10)
         (- *screen-height* 1)
         (format nil "turn ~D" *turn*)
         :width 10
         :halign :right)
  (print 0 (- *screen-height* 4)
         (format nil "F1: Refresh Config~%~
                      F2: Toggle Tiles~%~
                      F3: Toggle Mouse~%~
                      F4: Rebuild World"))
  (print 15 (- *screen-height* 4)
         (format nil "F5: Reveal Map~3%~
                      ESC: Quit")))

(defun draw ()
  (blt:clear)
  (draw-map)
  (run-render)
  (draw-mouse)
  (draw-status)
  (blt:refresh))


;;;; Input --------------------------------------------------------------------
(defun update-mouse-location ()
  (multiple-value-bind (x y)
      (blt:mouse)
    (setf *mouse-x* (floor x 2)
          *mouse-y* (floor y 2))))


(defun mouse-coords ()
  (values *mouse-x* *mouse-y*))

(defun mouse-tile ()
  (multiple-value-call #'aref *map* (mouse-coords)))

(defun toggle-wall ()
  (multiple-value-bind (x y) (mouse-coords)
    (when (in-bounds-p x y)
      (setf (aref *map* x y)
            (if (typep (aref *map* x y) 'wall)
              (make-ground)
              (make-wall)))
      (recompute-fov)
      (rebuild-map-glyphs))))

(defun warp-player ()
  (multiple-value-bind (x y) (mouse-coords)
    (when (in-bounds-p x y)
      (setf (loc/x *player*) x
            (loc/y *player*) y)
      (recompute-fov))))


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

    (:move-up         (values t (player-move-or-attack  0 -1)))
    (:move-down       (values t (player-move-or-attack  0  1)))
    (:move-left       (values t (player-move-or-attack -1  0)))
    (:move-right      (values t (player-move-or-attack  1  0)))
    (:move-up-left    (values t (player-move-or-attack -1 -1)))
    (:move-up-right   (values t (player-move-or-attack  1 -1)))
    (:move-down-left  (values t (player-move-or-attack -1  1)))
    (:move-down-right (values t (player-move-or-attack  1  1)))

    (:quit (setf *running* (values nil nil)))))

(defun handle-events ()
  (iterate
    (for event = (read-event))
    (until (eql event :done))
    (when event
      (for (values needs-redraw needs-tick) = (handle-event event))
      (when needs-tick (tick))
      (oring needs-redraw))))


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
