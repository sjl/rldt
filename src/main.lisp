(in-package :rl)

;;;; Parameters ---------------------------------------------------------------
(defvar *running* t)

(defparameter *screen-width* 64)
(defparameter *screen-height* 40)
(defparameter *cell-size* 16)

(defparameter *map-width* *screen-width*)
(defparameter *map-height* (- *screen-height* 5))

(defparameter *max-monsters-per-room* 3)

(defconstant +color-light-wall+ (blt:white :value 0.7))
(defconstant +color-light-ground+ (blt:white :value 0.5))
(defconstant +color-dark-wall+ (blt:white :value 0.4))
(defconstant +color-dark-ground+ (blt:white :value 0.3))


(defparameter *layer-bg* 0)
(defparameter *layer-items* 1)
(defparameter *layer-corpses* 2)
(defparameter *layer-mobs* 3)
(defparameter *layer-player* 4)
(defparameter *layer-mouse* 5)
(defparameter *layer-overlay* 10)


(defvar *mouse-x* 0)
(defvar *mouse-y* 0)

(defvar *enable-tiles* nil)

(defvar *game-state* :running)

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
  (color :initform (blt:white))
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


;;; Solid
(define-aspect solid)


;;; Brains
(define-aspect brain)


;;; Destructible
(define-aspect destructible
  (current-hp :type integer :initform 1)
  (maximum-hp :type integer :initform 1)
  (armor :type integer :initform 0)
  (on-death :type function))


(defmethod initialize-instance :after ((e destructible) &key)
  (setf (destructible/current-hp e)
        (destructible/maximum-hp e)))

(defun adjust-health (entity amount)
  (incf (destructible/current-hp entity) amount)
  (clampf (destructible/current-hp entity)
          0 (destructible/maximum-hp entity)))

(defun kill (entity)
  (when (slot-boundp entity 'destructible/on-death)
    (funcall (destructible/on-death entity) entity))
  (create-corpse-from entity)
  (destroy-entity entity))

(defun hurt (entity amount)
  (adjust-health entity (- amount))
  (when (zerop (destructible/current-hp entity))
    (kill entity)))

(defun heal (entity amount)
  (adjust-health entity amount))


;;; Attacker
(define-aspect attacker
  (damage :type list :initform '(1 6))
  (penetration :type integer :initform 0))

(defun attack (attacker defender)
  (let* ((damage (apply #'d (attacker/damage attacker)))
         (armor (max 0 (- (destructible/armor defender)
                          (attacker/penetration attacker))))
         (total (- damage armor)))
    (if (zerop total)
      (message (format nil "~A hit ~A, but didn't get past the armor."
                       (flavor/name attacker)
                       (flavor/name defender)))
      (message (format nil "~A hit ~(~A~) for ~D damage!"
                       (flavor/name attacker)
                       (flavor/name defender)
                       total)))
    (hurt defender total)))

(defun alivep (entity)
  (plusp (destructible/current-hp entity)))


;;;; Entities -----------------------------------------------------------------

;;;; Corpses
(define-entity corpse (loc renderable flavor))

(defun create-corpse-from (entity)
  (create-entity 'corpse
    :loc/x (loc/x entity)
    :loc/y (loc/y entity)
    :renderable/glyph #\%
    :renderable/color (renderable/color entity)
    :renderable/layer *layer-corpses*
    :flavor/name (format nil "~A corpse" (flavor/name entity))))


;;;; Monsters
(define-entity monster (loc renderable flavor brain destructible attacker solid))

(defun create-goblin (x y)
  (create-entity 'monster
    :loc/x x
    :loc/y y
    :renderable/glyph #\g
    :renderable/color (blt:rgba 0 255 0)
    :flavor/name "A goblin"
    :destructible/maximum-hp 10
    :attacker/damage '(1 4 -1)))

(defun create-orc (x y)
  (create-entity 'monster
    :loc/x x
    :loc/y y
    :renderable/glyph #\o
    :renderable/color (blt:rgba 0 200 80)
    :flavor/name "An orc"
    :destructible/maximum-hp 15
    :destructible/armor 1
    :attacker/penetration 1
    :attacker/damage '(1 6)))

(defun create-troll (x y)
  (create-entity 'monster
    :loc/x x
    :loc/y y
    :renderable/glyph #\T
    :renderable/color (blt:rgba 0 180 0)
    :flavor/name "A troll"
    :destructible/maximum-hp 40
    :attacker/damage '(2 3 +1)))


;;; Player
(define-entity player (loc renderable flavor destructible attacker solid))

(defun kill-player (player)
  (declare (ignore player))
  (setf *game-state* :dead))

(defun create-player ()
  (create-entity 'player
    :loc/x 0 ; loc will be set later, after map generation
    :loc/y 0
    :renderable/glyph #\@
    :renderable/layer *layer-player*
    :flavor/name "You"
    :destructible/maximum-hp 30
    :destructible/on-death #'kill-player
    :attacker/damage '(1 8)))


;;;; Game Logic ---------------------------------------------------------------
(defvar *player* nil)


(defun objects-at (x y)
  (when (in-bounds-p x y)
    (lref x y)))

(defun object-at (x y)
  (first (objects-at x y)))


(defun blockedp (x y)
  (ensure-boolean (or (tile-blocks-movement-p (map-ref? x y))
                      (find-if #'solid? (objects-at x y)))))

(defun move-to (entity x y)
  (if (or (not (in-bounds-p x y))
          (blockedp x y))
    nil
    (progn (loc-move-entity entity x y)
           t)))

(defun move (entity dx dy)
  (move-to entity
           (+ dx (loc/x entity))
           (+ dy (loc/y entity))))


(defun place-player-initial ()
  (multiple-value-call #'loc-move-entity *player*
    (rl.bsp::random-room-point (random-room))))


(defun player-move-to (x y)
  (prog1
      (move-to *player* x y)
    (recompute-fov)))

(defun player-attack (target)
  (attack *player* target)
  t)

(defun player-move-or-attack (dx dy)
  (let* ((tx (+ (loc/x *player*) dx))
         (ty (+ (loc/y *player*) dy))
         (target (object-at tx ty)))
    (if (and target (destructible? target))
      (player-attack target)
      (player-move-to tx ty))))


(defun distance (x1 y1 x2 y2)
  (sqrt (+ (square (- x1 x2))
           (square (- y1 y2)))))

(defun distance-to (entity other)
  (distance (loc/x entity)
            (loc/y entity)
            (loc/x other)
            (loc/y other)))

(defun delta-toward (ox oy tx ty)
  (let* ((dx (- tx ox))
         (dy (- ty oy))
         (distance (sqrt (+ (square dx) (square dy)))))
    (values (round (/ dx distance))
            (round (/ dy distance)))))

(defun move-toward (entity x y)
  (multiple-value-call #'move entity
    (delta-toward (loc/x entity)
                  (loc/y entity)
                  x y)))


;;;; Systems ------------------------------------------------------------------

;;; Rendering
(defun draw-entity (entity)
  (let ((x (loc/x entity))
        (y (loc/y entity)))
    (when (visiblep x y)
      (setf (blt:layer) (renderable/layer entity)
            (blt:font) "tile"
            (blt:composition) t
            (blt:color) (blt:black)
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
  (when (visiblep (loc/x entity) (loc/y entity))
    (cond
      ((>= (distance-to entity *player*) 2)
       (move-toward entity (loc/x *player*) (loc/y *player*)))
      ((alivep *player*)
       (attack entity *player*)))))

(defun tick ()
  (incf *turn*)
  (run-act))


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
  (setf *game-state* :running
        *messages* nil)
  (clear-entities)
  (initialize-locations)
  (generate-map)
  (generate-player)
  (generate-mobs)
  (place-player-initial)
  (rebuild-map-glyphs)
  (recompute-fov)
  (message "Welcome to the game, good luck!")
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


;;;; Message Log --------------------------------------------------------------
(defvar *messages* nil)

(defparameter *messages-width* 30)
(defparameter *messages-height* 4)

(defun message (string)
  (push string *messages*)
  (zapf *messages* (take *messages-height* %)))


;;;; Pathfinding --------------------------------------------------------------
(defun find-path (x1 y1 x2 y2)
  (when (and (in-bounds-p x1 y1)
             (in-bounds-p x2 y2))
    (let ((start (complex x1 y1)) ; ugly vec2s
          (goal (complex x2 y2)))
      (mapcar (lambda (point)
                (cons (realpart point) (imagpart point)))
              (a* :start start
                  :neighbors (lambda (coords)
                               (iterate
                                 (for (dx dy) :within-radius 1 :skip-origin t)
                                 (for x = (+ (realpart coords) dx))
                                 (for y = (+ (imagpart coords) dy))
                                 (when (and (in-bounds-p x y)
                                            (not (blockedp x y)))
                                   (collect (complex x y)))))
                  :goal-p (lambda (coords)
                            (= coords goal))
                  :cost (lambda (c1 c2)
                          (abs (- c1 c2)))
                  :heuristic (lambda (coords)
                               (abs (- coords goal))))))))


;;;; Mouse --------------------------------------------------------------------
(defun update-mouse-location ()
  (multiple-value-bind (x y)
      (blt:mouse)
    (setf *mouse-x* (floor x 2)
          *mouse-y* (floor y 2))))


(defun mouse-coords ()
  (values *mouse-x* *mouse-y*))

(defun mouse-tile ()
  (multiple-value-call #'aref *map* (mouse-coords)))

(defun mouse-objects ()
  (lref *mouse-x* *mouse-y*))


;;;; God Mode -----------------------------------------------------------------
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


;;;; Rendering ----------------------------------------------------------------
(defparameter *bar-width* 10)

(defun tile-color (tile visible?)
  (etypecase tile
    (wall (if visible?
            +color-light-wall+
            +color-dark-wall+))
    (ground (if visible?
              +color-light-ground+
              +color-dark-ground+))))


(defun draw-mousepath ()
  (setf (blt:color) (blt:red :alpha 0.8)
        (blt:font) "")
  (iterate (for (x . y) :in (find-path (loc/x *player*) (loc/y *player*)
                                       *mouse-x* *mouse-y*))
           (setf (cell-char x y) #\full_block)))

(defun draw-mouselook ()
  (draw-mousepath)
  (when (and (in-bounds-p *mouse-x* *mouse-y*)
             (visiblep *mouse-x* *mouse-y*))
    (setf (blt:layer) *layer-mouse*
          (blt:color) (blt:white :alpha 0.4)
          (blt:font) ""
          (cell-char *mouse-x* *mouse-y*) #\full_block)
    (setf (blt:color) (blt:white)
          (blt:font) "text")
    (print 0 (- *screen-height* 5)
           (format nil "~{~A~^, ~}"
                   (-<> (mouse-objects)
                     (remove-if-not #'flavor? <>)
                     (mapcar #'flavor/name <>))))))

(defun draw-messages ()
  (print (1+ *bar-width*) (- *screen-height* 4)
         (format nil "~{~A~^~%~}"
                 (reverse *messages*))
         :width *messages-width*
         :height *messages-height*
         :valign :bottom))

(defun draw-bar (x y width string value maximum bar-color background-color text-color)
  (setf (blt:font) "text"
        (blt:color) background-color)
  (print x y (make-string (* 2 width) :initial-element #\full_block))
  (setf (blt:color) bar-color)
  (print x y (make-string (floor (map-range 0 maximum 0 (* 2 width) value))
                          :initial-element #\full_block))
  (setf (blt:color) text-color
        (blt:composition) t)
  (print x y string :width width :halign :center)
  (setf (blt:composition) nil))

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

(defun draw-status ()
  (setf (blt:color) (blt:white)
        (blt:font) "text")
  (print (- *screen-width* 10)
         (- *screen-height* 1)
         (format nil "turn ~D" *turn*)
         :width 10
         :halign :right)
  (let ((cur-hp (destructible/current-hp *player*))
        (max-hp (destructible/maximum-hp *player*)))
    (draw-bar 0 (- *screen-height* 4) *bar-width*
              (format nil "HP: ~D/~D" cur-hp max-hp)
              cur-hp
              max-hp
              (blt:green :value 0.8)
              (blt:red :value 0.7)
              (blt:white)))
  (let ((cur-xp 50)
        (max-xp 100))
    (draw-bar 0 (- *screen-height* 3) *bar-width*
              (format nil "XP: ~D/~D" cur-xp max-xp)
              cur-xp
              max-xp
              (blt:yellow :value 0.8)
              (blt:gray)
              (blt:white)))
  (draw-messages))

(defun draw-state ()
  (ecase *game-state*
    (:running nil)
    (:dead
     (setf (blt:font) "simple"
           (blt:layer) *layer-overlay*)
     (blt:draw-box 0 0 30 6
                   :contents (format nil "[font=text]You have died.~%Press space to play again.")
                   :background-color nil :border nil))))

(defun draw ()
  (blt:clear)
  (draw-map)
  (run-render)
  (draw-status)
  (draw-mouselook)
  (draw-state)
  (blt:refresh))


;;;; Input --------------------------------------------------------------------
(defun read-event ()
  (if (blt:has-input-p)
    (let ((event (blt:read)))
      (blt:key-case event
        (:f1 :refresh-config)
        (:f2 :flip-tiles)
        (:f3 :reveal-map)
        (:f4 :regenerate-world)

        (:escape :quit)
        (:close :quit)

        (:mouse-move :mouse-move)
        (:mouse-left :toggle-wall)
        (:mouse-right :warp-player)

        (t (ecase *game-state*
             (:running
              (blt:key-case event
                (:up :move-up)
                (:down :move-down)
                (:left :move-left)
                (:right :move-right)

                (:numpad-1 :move-down-left)
                (:numpad-2 :move-down)
                (:numpad-3 :move-down-right)
                (:numpad-4 :move-left)
                (:numpad-5 :wait)
                (:numpad-6 :move-right)
                (:numpad-7 :move-up-left)
                (:numpad-8 :move-up)
                (:numpad-9 :move-up-right)))
             (:dead
              (blt:key-case event
                (:space :regenerate-world)))))))
    :done))

(defun handle-event (event)
  "Handle the given event.

  Returns two values: whether the screen needs to be redrawn, and whether the
  game time should be ticked.

  "
  (ecase event
    (:refresh-config (config) (values t nil))
    (:flip-tiles (notf *enable-tiles*) (config) (values t))
    (:mouse-move (update-mouse-location) (values t nil))
    (:reveal-map (reveal-map) (values t nil))
    (:toggle-wall (toggle-wall) (values t nil))
    (:regenerate-world (generate) (values t nil))
    (:warp-player (warp-player) (values t nil))
    (:quit (setf *running* (values nil nil)))

    (:move-up         (values t (player-move-or-attack  0 -1)))
    (:move-down       (values t (player-move-or-attack  0  1)))
    (:move-left       (values t (player-move-or-attack -1  0)))
    (:move-right      (values t (player-move-or-attack  1  0)))
    (:move-up-left    (values t (player-move-or-attack -1 -1)))
    (:move-up-right   (values t (player-move-or-attack  1 -1)))
    (:move-down-left  (values t (player-move-or-attack -1  1)))
    (:move-down-right (values t (player-move-or-attack  1  1)))

    (:wait (heal *player* 1) (values t t))))

(defun handle-events ()
  (iterate
    (for event = (read-event))
    (until (eql event :done))
    (when event
      (for (values needs-redraw needs-tick) = (handle-event event))
      (when needs-tick (tick))
      (oring needs-redraw))))


;;;; Config -------------------------------------------------------------------
(defun asset-path (filename)
  (concatenate 'string *assets-directory* filename))

(defun config-fonts ()
  (blt:set "simple font: ~A, size=~Dx~:*~D, spacing=1x1;"
           (asset-path "ProggySquare/ProggySquare.ttf")
           (floor *cell-size* 2))
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
  (blt:set "window.resizeable = true")
  (blt:set "output.vsync = true")
  (blt:set "input.filter = keyboard, mouse")
  (config-fonts)
  (setf *running* t))


;;;; Synchronized Queues ------------------------------------------------------
(defstruct (synchronized-queue (:include queue))
  (lock (bt:make-recursive-lock)))

(defun enqueue-sync (item queue)
  (bt:with-recursive-lock-held ((synchronized-queue-lock queue))
    (enqueue item queue)))

(defun dequeue-sync (queue)
  (bt:with-recursive-lock-held ((synchronized-queue-lock queue))
    (dequeue queue)))


;;;; State Machine ------------------------------------------------------------
(defun blit ()
  (blt:clear)
  (rl.panels::draw-panels)
  (blt:refresh))

(defun draw-generate-message (panel)
  (rl.panels::print panel 0 0 "[font=text]Generating dungeon..."
                    :width (blt:width)
                    :height (blt:height)
                    :halign :center
                    :valign :center))

(defun sample-generate ()
  (generate)
  (sleep 10.0))

(defun read-event ()
  (when (blt:has-input-p)
    (blt:read)))

(defparameter *event-queue* nil)
(defparameter *dirty* t)
(defvar *game-thread* nil)

(defun mark-dirty ()
  (setf *dirty* t))

(defun handle-ui-event (event)
  (when (rl.panels::handle-event-for-panels event)
    (mark-dirty))
  (blt:key-case event
    (:mouse-move (update-mouse-location) (mark-dirty))
    (:close (setf *running* nil))))

(defun pump-events ()
  (iterate
    (for event = (read-event))
    (while event)
    (handle-ui-event event)
    (enqueue-sync event *event-queue*)))

(defun draw-or-sleep ()
  (if *dirty*
    (progn
      (setf *dirty* nil)
      (blit))
    (progn
      (blt:sleep 1/100))))

(defun ui-thread ()
  (iterate
    (draw-or-sleep)
    (pump-events)
    (while *running*)))

(defun game-thread ()
  (unwind-protect
      (state-generate)
    (setf *running* nil)))

(defun state-generate ()
  (rl.panels::with-panel (p (rl.panels::stretch) #'draw-generate-message)
    (sample-generate)))

(defun run-game ()
  (blt:with-terminal
    (setf (blt:color) (blt:white)
          *event-queue* (make-synchronized-queue)
          *running* t
          *dirty* t)
    (config)
    (setf *game-thread* (bt:make-thread #'game-thread))
    (ui-thread)))

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
