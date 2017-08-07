(in-package :rl)

;;;; Parameters ---------------------------------------------------------------
(defvar *running* t)

(defparameter *screen-width* 64)
(defparameter *screen-height* 40)
(defparameter *cell-size* 16)

(defparameter *map-width* *screen-width*)
(defparameter *map-height* (- *screen-height* 5))

(defparameter *max-monsters-per-room* 3)
(defparameter *max-items-per-room* 2)
(defparameter *lightning-range* 8)
(defparameter *confusion-range* 5)
(defparameter *confusion-duration* 10)
(defconstant +color-light-wall+ (blt:white :value 0.7))
(defconstant +color-light-ground+ (blt:white :value 0.5))
(defconstant +color-dark-wall+ (blt:white :value 0.4))
(defconstant +color-dark-ground+ (blt:white :value 0.3))

(defparameter *status-width* 10)
(defparameter *status-height* 5)

(defparameter *layer-tiles* 0)
(defparameter *layer-items* 1)
(defparameter *layer-corpses* 2)
(defparameter *layer-mobs* 3)
(defparameter *layer-player* 4)

(defvar *map-panel* nil)
(defvar *status-panel* nil)
(defvar *message-panel* nil)

(defvar *mouse-x* 0)
(defvar *mouse-y* 0)

(defvar *enable-tiles* nil)

(defvar *game-state* :running)
(defvar *world-ready* nil)

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

;;;; Renderable
(define-aspect renderable
  (glyph :type character :initform #\?)
  (color :initform (blt:white))
  (layer :type layer :initform *layer-mobs*))


;;;; Flavor
(define-aspect flavor
  (name :type string :initform "Something"))


;;;; Locations
(defvar *locations* nil)

(define-aspect loc
  (x :type (or null coordinate))
  (y :type (or null coordinate)))

(defun initialize-locations ()
  (setf *locations* (make-array (list *map-width* *map-height*)
                      :initial-element nil
                      :element-type 'list)))

(defun loc-insert-entity (e)
  (when (loc/x e)
    (push e (aref *locations* (loc/x e) (loc/y e)))))

(defun loc-remove-entity (e)
  (when (loc/y e)
    (zapf (aref *locations* (loc/x e) (loc/y e))
          (delete e %))))

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


;;;; Solid
(define-aspect solid)


;;;; Brains
(define-aspect brain
  (ai-function :initform 'simple-ai))


;;;; Destructible
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


;;;; Attacker
(define-aspect attacker
  (damage :type list :initform '(1 6))
  (penetration :type integer :initform 0))

(defun attack (attacker defender)
  (let* ((damage (apply #'d (attacker/damage attacker)))
         (armor (max 0 (- (destructible/armor defender)
                          (attacker/penetration attacker))))
         (total (- damage armor)))
    (if (zerop total)
      (message "~A hit ~A, but didn't get past the armor."
               (flavor/name attacker)
               (flavor/name defender))
      (message "~A hit ~(~A~) for ~D damage!"
               (flavor/name attacker)
               (flavor/name defender)
               total))
    (hurt defender total)))

(defun alivep (entity)
  (plusp (destructible/current-hp entity)))


;;;; Containers
(define-aspect container
  (contents :type list :initform nil)
  (capacity :type fixnum :initform 26))


(defun container-full-p (container)
  (= (length (container/contents container))
     (container/capacity container)))

(defun container-empty-p (container)
  (null (container/contents container)))


;;;; Containable
(define-aspect containable
  (container :type (or null container) :initform nil))


(defun remove-item-from-world (item)
  (when (loc? item)
    (loc-remove-entity item)
    (setf (loc/x item) nil
          (loc/y item) nil)))

(defun return-item-to-world (item container)
  (when (and (loc? item) (loc? container))
    (setf (loc/x item) (loc/x container)
          (loc/y item) (loc/y container))
    (loc-insert-entity item)))

(defun insert-item (item container)
  (assert (not (container-full-p container)))
  (push item (container/contents container))
  (setf (containable/container item) container)
  (remove-item-from-world item))

(defun remove-item (item container)
  (assert (member item (container/contents container)))
  (removef (container/contents container) item)
  (setf (containable/container item) container)
  (return-item-to-world item container))


(defmethod entity-destroyed :before ((entity containable))
  (when-let* ((container (containable/container entity)))
    (remove-item entity container)))


;;;; Usable
(define-aspect usable
  (effect :type function))

(defun use (item user)
  (let ((valid (funcall (usable/effect item) user)))
    (when valid
      (destroy-entity item))
    valid))


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


;;;; Player
(define-entity player
  (loc renderable flavor destructible attacker solid container))

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


;;;; Healing Potion
(define-entity healing-potion (loc renderable flavor containable usable))

(defun create-healing-potion (x y)
  (create-entity 'healing-potion
    :loc/x x
    :loc/y y
    :renderable/glyph #\!
    :renderable/color (blt:purple)
    :renderable/layer *layer-items*
    :flavor/name "a healing potion"
    :usable/effect 'use-healing-potion))

(defun use-healing-potion (user)
  (when (destructible? user)
    (heal target (d 3 6 +1))))


;;;; Scroll of Lightning
(define-entity scroll-of-lightning (loc renderable flavor containable usable))

(defun create-scroll-of-lightning (x y)
  (create-entity 'healing-potion
    :loc/x x
    :loc/y y
    :renderable/glyph #\?
    :renderable/color (blt:yellow :saturation 0.3)
    :renderable/layer *layer-items*
    :flavor/name "a scroll of lightning"
    :usable/effect 'use-scroll-of-lightning))

(defun use-scroll-of-lightning (user)
  (declare (ignore user))
  (let ((target (closest-monster *lightning-range*))
        (damage (d 4 10)))
    (if target
      (progn (message "[color=yellow]BZZZZZZT![/color] ~A was zapped for ~D damage."
                      (flavor/name target)
                      damage)
             (hurt target damage)
             t)
      (progn (message "There are no targets in range.")
             nil))))


;;;; Scroll of Confusion
(define-entity scroll-of-confusion (loc renderable flavor containable usable))

(defun create-scroll-of-confusion (x y)
  (create-entity 'healing-potion
    :loc/x x
    :loc/y y
    :renderable/glyph #\?
    :renderable/color (blt:yellow :saturation 0.3)
    :renderable/layer *layer-items*
    :flavor/name "a scroll of confusion"
    :usable/effect 'use-scroll-of-confusion))

(defun use-scroll-of-confusion (user)
  (declare (ignore user))
  (let ((target (closest-monster *confusion-range*)))
    (if target
      (progn (message "~A begins to wander around aimlessly."
                      (flavor/name target))
             (swap-brain target 'confused-ai *confusion-duration*)
             t)
      (progn (message "There are no targets in range.")
             nil))))


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

(define-system render ((entity renderable loc))
  (draw-entity entity))


;;; AI
(defvar *turn* 0)

(defun simple-ai (entity)
  (when (visiblep (loc/x entity) (loc/y entity))
    (cond
      ((>= (distance-to entity *player*) 2)
       (move-toward entity (loc/x *player*) (loc/y *player*)))
      ((alivep *player*)
       (attack entity *player*)))))

(defun confused-ai (entity)
  (move entity
        (random-range-inclusive -1 1)
        (random-range-inclusive -1 1)))

(defun swap-brain (entity new-ai turns)
  (let ((old-ai (brain/ai-function entity))
        (remaining turns))
    (setf (brain/ai-function entity)
          (lambda (entity)
            (prog1 (funcall new-ai entity)
              (when (zerop (decf remaining))
                (setf (brain/ai-function entity) old-ai)))))))


(define-system act ((entity brain))
  (funcall (brain/ai-function entity) entity))


(defun tick ()
  (incf *turn*)
  (run-act))


;;;; Generation ---------------------------------------------------------------
(defparameter *monster-selection*
  (make-weightlist '((60 . create-goblin)
                     (30 . create-orc)
                     (10 . create-troll))))

(defparameter *item-selection*
  (make-weightlist '((60 . create-healing-potion)
                     (15 . create-scroll-of-lightning)
                     (15 . create-scroll-of-confusion))))


(defun create-random-monster (x y)
  (funcall (weightlist-random *monster-selection*) x y))

(defun create-random-item (x y)
  (funcall (weightlist-random *item-selection*) x y))


(defun populate-room-with-monsters (room)
  (dorepeat (random (1+ *max-monsters-per-room*))
    (multiple-value-bind (x y)
        (rl.bsp::random-room-point room)
      (unless (blockedp x y)
        (create-random-monster x y)))))

(defun populate-room-with-items (room)
  (dorepeat (random (1+ *max-items-per-room*))
    (multiple-value-call #'create-random-item
      (rl.bsp::random-room-point room))))

(defun populate-room (room)
  (populate-room-with-monsters room)
  (populate-room-with-items room))

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
  (sleep 0.1)
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


(defun closest-monster (range)
  (iterate
    (with px = (loc/x *player*))
    (with py = (loc/y *player*))
    (for entity :in (map-entities #'identity 'monster))
    (for ex = (loc/x entity))
    (for ey = (loc/y entity))
    (when ex
      (for distance = (distance px py ex ey))
      (when (and (not (eq entity *player*))
                 (visiblep ex ey)
                 (<= distance range))
        (finding entity :minimizing distance)))))


;;;; Message Log --------------------------------------------------------------
(defvar *messages* nil)

(defparameter *messages-width* 30)
(defparameter *messages-height* 4)

(defun message (string &rest format-arguments)
  (push (apply #'format nil string format-arguments) *messages*)
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
                                 (for (x y) :within-radius 1 :skip-origin t
                                      :origin ((realpart coords)
                                               (imagpart coords)))
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
  (config-fonts))


;;;; UI -----------------------------------------------------------------------
(defun blit ()
  (blt:clear)
  (rl.panels::draw-panels)
  (blt:refresh))

(defun read-event ()
  (when (blt:has-input-p)
    (blt:read)))

(defun handle-basic-event (event)
  (let ((panels? (rl.panels::handle-event-for-panels event))
        (other? (blt:key-case event
                  (:mouse-move (update-mouse-location) t)
                  (:f7 (cerror "It's fine." "BORK") nil) ; explode
                  (:f8 t) ; force blit
                  (:close (setf *running* nil) t))))
    (or panels? other?)))

(defun get-event ()
  (when-let* ((event (read-event)))
    (when (handle-basic-event event)
      (blit))
    event))

(defun get-event-blocking ()
  (iterate
    (for event = (get-event))
    (if event
      (return event)
      (blt:sleep 1/60))))

(defmacro event-case (&body clauses)
  `(blt:key-case (get-event) ,@clauses))

(defmacro event-case-blocking (&body clauses)
  `(iterate (thereis (blt:key-case (get-event-blocking) ,@clauses))))


;;;; State Machine ------------------------------------------------------------
(defmacro define-state-machine-macros ()
  (with-gensyms (next-state transition recur main)
    `(progn
       (defmacro define-state (state-name &body body)
         `(defun ,state-name ()
            (let (,',next-state)
              (tagbody
                (go ,',main)
                ,',recur (return-from ,state-name (funcall ',state-name))
                ,',transition (return-from ,state-name (funcall ,',next-state))
                ,',main ,@body))))

       (defmacro transition (next-state)
         `(progn (setf ,',next-state ',next-state) (go ,',transition)))

       (defmacro reenter ()
         `(go ,',recur)))))

(define-state-machine-macros)


;;;; Drawing ------------------------------------------------------------------
(defun centered (width height)
  (lambda (w h)
    (values (floor (- w width) 2)
            (floor (- h height) 2)
            width height)))

(defun draw-bar (panel x y width string value maximum bar-color background-color text-color)
  (setf (blt:font) "text"
        (blt:color) background-color)
  (rl.panels::print panel (* 2 x) (* 2 y)
                    (make-string (* 2 width) :initial-element #\full_block))
  (setf (blt:color) bar-color)
  (rl.panels::print panel (* 2 x) (* 2 y)
                    (make-string (floor (map-range 0 maximum 0 (* 2 width) value))
                                 :initial-element #\full_block))
  (setf (blt:color) text-color
        (blt:composition) t)
  (rl.panels::print panel (* 2 x) (* 2 y) string :width (* 2 width) :halign :center)
  (setf (blt:composition) nil))


(defun draw-message-box (contents width height panel)
  (setf (blt:font) "text")
  (rl.panels::print panel 1 1 contents
                    :width (- width 2)
                    :height (- height 2)
                    :halign :center
                    :valign :center))


(defmacro with-message-box ((width height contents) &body body)
  (once-only (width height)
    `(rl.panels::with-panel (,(gensym)
                             (centered ,width ,height)
                             (curry 'draw-message-box ,contents ,width ,height)
                             :background-color (blt:black :alpha 0.7)
                             :border :light)
       (blit)
       ,@body)))


(defun draw-generate-message (panel)
  (setf (blt:font) "text")
  (rl.panels::print panel 0 0 "Generating world..."
                    :width (rl.panels::panel-width panel)
                    :height (rl.panels::panel-height panel)
                    :halign :center
                    :valign :center))


(defun tile-color (tile visible?)
  (etypecase tile
    (wall (if visible?
            +color-light-wall+
            +color-dark-wall+))
    (ground (if visible?
              +color-light-ground+
              +color-dark-ground+))))

(defun draw-entity (entity)
  (let ((x (loc/x entity))
        (y (loc/y entity)))
    (when (and x y (visiblep x y))
      (incf (blt:layer) (renderable/layer entity))
      (setf (blt:font) "tile"
            (blt:composition) t
            (blt:color) (blt:black)
            (rl.panels::cell-char *map-panel* (* 2 x) (* 2 y)) #\full_block
            (blt:color) (renderable/color entity)
            (rl.panels::cell-char *map-panel* (* 2 x) (* 2 y)) (renderable/glyph entity)
            (blt:composition) nil)
      (decf (blt:layer) (renderable/layer entity))))
  (values))

(defun draw-map (panel)
  (when *world-ready*
    (setf (blt:font) "tile")
    (flet ((draw-tile (tile x y visible)
             (setf (blt:color) (tile-color tile visible)
                   (rl.panels::cell-char panel (* 2 x) (* 2 y)) (tile-glyph tile))))
      ;; todo: pull the exploration part of this out into the main game logic
      (iterate (for (tile x y) :in-array *map*)
               (for visible = (aref *fov-map* x y))
               (for seen = (tile-seen tile))
               (when (and visible (not seen))
                 (setf (tile-seen tile) t))
               (when (or visible seen)
                 (draw-tile tile x y visible))))
    (run-render)))


(defun draw-messages (panel)
  (when *world-ready*
    (setf (blt:font) "text")
    (rl.panels::print panel 0 (* 2 1)
                      (format nil "~{~A~^~%~}"
                              (reverse *messages*))
                      :width (* 2 *messages-width*)
                      :height (* 2 (1- *status-height*))
                      :valign :bottom)))


(defun draw-status-look (panel)
  (when (and (in-bounds-p *mouse-x* *mouse-y*)
             (visiblep *mouse-x* *mouse-y*))
    ;; todo: make an overlay panel
    ;; (setf (blt:color) (blt:white :alpha 0.4)
    ;;       (blt:font) ""
    ;;       (cell-char *mouse-x* *mouse-y*) #\full_block)
    (setf (blt:color) (blt:white)
          (blt:font) "text")
    (rl.panels::print panel 0 0
           (format nil "~{~A~^, ~}"
                   (-<> (mouse-objects)
                     (remove-if-not #'flavor? <>)
                     (mapcar #'flavor/name <>))))))

(defun draw-status (panel)
  (when *world-ready*
    (draw-status-look panel)
    (setf (blt:color) (blt:white)
          (blt:font) "text")
    (let ((cur-hp (destructible/current-hp *player*))
          (max-hp (destructible/maximum-hp *player*)))
      (draw-bar panel 0 1 *status-width*
                (format nil "HP: ~D/~D" cur-hp max-hp)
                cur-hp
                max-hp
                (blt:green :value 0.8)
                (blt:red :value 0.7)
                (blt:white)))
    (let ((cur-xp 50)
          (max-xp 100))
      (draw-bar panel 0 2 *status-width*
                (format nil "XP: ~D/~D" cur-xp max-xp)
                cur-xp
                max-xp
                (blt:yellow :value 0.8)
                (blt:gray)
                (blt:white)))))


(defun press-space-or-escape ()
  (event-case-blocking
    ((or :escape :space) t)))


(defun get-selection-event ()
  ; ugh blt
  (event-case-blocking
    (:a 0)
    (:b 1)
    (:c 2)
    (:d 3)
    (:e 4)
    (:f 5)
    (:g 6)
    (:h 7)
    (:i 8)
    (:j 9)
    (:k 10)
    (:l 11)
    (:m 12)
    (:n 13)
    (:o 14)
    (:p 15)
    (:q 16)
    (:r 17)
    (:s 18)
    (:t 19)
    (:u 20)
    (:v 21)
    (:w 22)
    (:x 23)
    (:y 24)
    (:z 25)
    (:escape :nevermind)))

(defun selection-menu (prompt choices format-function)
  (let* ((number-of-choices (length choices))
         (width)
         (height)
         (text (with-output-to-string (s)
                 (format s "~A~2%" prompt)
                 (iterate
                   (for h :from 0)
                   (for choice :in choices)
                   (for choice-string = (funcall format-function choice))
                   (for key :in-vector "abcdefghijklmnopqrstuvwxyz")
                   (for string = (format nil "~A: ~A~%" key choice-string))
                   (maximizing (1- (length string)) :into w)
                   (princ string s)
                   (finally (setf width (max w (length prompt))
                                  height (+ 2 h)))))))
    (rl.panels::with-panel (panel (centered (+ 4 width) (+ 4 (* 2 height)))
                                  (lambda (panel)
                                    (setf (blt:font) "text")
                                    (rl.panels::print panel 2 2 text))
                                  :border :double
                                  :background-color (blt:black))
      (iterate
        (blit)
        (for selection = (get-selection-event))
        (cond
          ((eq selection :nevermind)
           (return-from selection-menu nil))
          ((< selection number-of-choices)
           (return-from selection-menu (elt choices selection))))))))

(defmacro choose-and-do
    ((prompt failure-message choice choices formatter) &body body)
  (with-gensyms (choices-list)
    `(let ((,choices-list ,choices))
       (if (null ,choices-list)
         (with-message-box (40 4 ,failure-message)
           (blit)
           (press-space-or-escape)
           (transition state-play))
         (let ((,choice (selection-menu ,prompt ,choices-list ,formatter)))
           (if ,choice
             (progn ,@body)
             (transition state-play)))))))


;;;; States -------------------------------------------------------------------
;;;; Dead
(define-state state-dead
  (with-message-box (20 6 "You have died.")
    (event-case-blocking
      ((or :escape :space) (transition state-generate)))))


;;;; Tick
(define-state state-tick
  (tick)
  (transition state-play))


;;;; Use
(defun find-items-to-use ()
  (remove-if-not #'usable? (container/contents *player*)))


(define-state state-use
  (choose-and-do ("What would you like to use?"
                  "You don't have anything you can use."
                  choice (find-items-to-use) #'flavor/name)
    (if (use choice *player*)
      (transition state-tick)
      (transition state-play))))


;;;; Pick Up
(defun find-items-to-pick-up ()
  (-<> (lref (loc/x *player*) (loc/y *player*))
    (remove-if-not #'containable? <>)))


(define-state state-pick-up
  (choose-and-do ("What would you like to pick up?"
                  "There's nothing to pick up."
                  choice (find-items-to-pick-up) #'flavor/name)
    (insert-item choice *player*)
    (transition state-tick)))


;;;; Drop
(define-state state-drop
  (choose-and-do ("What would you like to drop?"
                  "You don't have anything to drop."
                  choice (container/contents *player*) #'flavor/name)
    (remove-item choice *player*)
    (transition state-tick)))


;;;; Play
(define-state state-inventory
  (choose-and-do ("You are carrying:"
                  "You're not carrying anything."
                  choice (container/contents *player*) #'flavor/name)
    (transition state-play)))


;;;; Play
(defun get-action-play ()
  (event-case-blocking
    (:f1 :refresh-config)
    (:f2 :flip-tiles)
    (:f3 :reveal-map)
    (:f4 :regenerate-world)

    ((or :close (:q :shift)) :quit)

    (:mouse-left :toggle-wall)
    (:mouse-right :warp-player)

    (:up :move-up)
    (:down :move-down)
    (:left :move-left)
    (:right :move-right)
    (:period :wait)

    (:numpad-1 :move-down-left)
    (:numpad-2 :move-down)
    (:numpad-3 :move-down-right)
    (:numpad-4 :move-left)
    (:numpad-5 :wait)
    (:numpad-6 :move-right)
    (:numpad-7 :move-up-left)
    (:numpad-8 :move-up)
    (:numpad-9 :move-up-right)

    (:d :drop)
    (:i :inventory)
    (:g :get)
    (:u :use)))

(defmacro tick-if (&body body)
  `(if (progn ,@body)
     (transition state-tick)
     (reenter)))


(define-state state-play
  (blit)

  (when (eq *game-state* :dead)
    (transition state-dead))

  (let ((action (get-action-play)))
    (ccase action
      (:refresh-config (config) (reenter))
      (:flip-tiles (notf *enable-tiles*) (config) (reenter))
      (:reveal-map (reveal-map) (reenter))
      (:toggle-wall (toggle-wall) (reenter))
      (:regenerate-world (transition state-generate))
      (:warp-player (warp-player) (reenter))

      (:quit (setf *running* nil))

      (:move-up         (tick-if (player-move-or-attack  0 -1)))
      (:move-down       (tick-if (player-move-or-attack  0  1)))
      (:move-left       (tick-if (player-move-or-attack -1  0)))
      (:move-right      (tick-if (player-move-or-attack  1  0)))
      (:move-up-left    (tick-if (player-move-or-attack -1 -1)))
      (:move-up-right   (tick-if (player-move-or-attack  1 -1)))
      (:move-down-left  (tick-if (player-move-or-attack -1  1)))
      (:move-down-right (tick-if (player-move-or-attack  1  1)))

      (:get (transition state-pick-up))
      (:drop (transition state-drop))
      (:inventory (transition state-inventory))
      (:use (transition state-use))

      (:wait (heal *player* 1) (transition state-tick)))))


;;;; Initialize
(define-state state-initialize
  (rl.panels::with-panels
    ((*map-panel* (lambda (width height)
                    (values 0 0 width (- height (* 2 *status-height*))))
                  'draw-map
                  :layers 6)
     (*status-panel* (lambda (width height)
                       (declare (ignore width))
                       (values 0 (- height (* 2 *status-height*))
                               (* 2 *status-width*) (* 2 *status-height*)))
                     'draw-status)
     (*message-panel* (lambda (width height)
                        (values (1+ (* 2 *status-width*))
                                (- height (* 2 *status-height*))
                                (- width (* 2 *status-width*) 1)
                                (* 2 *status-height*)))
                      'draw-messages))
    ;; Just call normally, so these panels stick around til we quit.
    (state-generate)))


;;;; Generate
(define-state state-generate
  (setf *world-ready* nil)
  (rl.panels::with-panel (p (rl.panels::stretch) 'draw-generate-message)
    (blit)
    (iterate (with generator = (bt:make-thread #'generate))
             (unless (bt:thread-alive-p generator)
               (setf *world-ready* t)
               (transition state-play))
             (get-event)
             (blt:sleep 1/60))))


;;;; Start
(define-state state-start
  (transition state-initialize))


;;;; Main ---------------------------------------------------------------------
(defun run ()
  (blt:with-terminal
    (setf (blt:color) (blt:white)
          *running* t)
    (config)
    (blit)
    (rl.panels::initialize-panels)
    (state-start)))


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
