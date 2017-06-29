(in-package :rl)

;;;; Parameters ---------------------------------------------------------------
(defvar *running* t)

(defparameter *screen-width* 40)
(defparameter *screen-height* 40)

(defparameter *layer-bg* 0)
(defparameter *layer-mobs* 1)


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
  (incf (object-x object) dx)
  (incf (object-y object) dy)
  (values))

(defun draw-object (object)
  (with-object (object)
    (setf (blt:layer) layer
          (blt:color) color
          (blt:cell-char x y) glyph))
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

(defparameter *npc*
  (make-object 4 4
               :glyph #\F
               :color (blt:hsva 0.3 0.8 1.0)
               :layer *layer-mobs*))

(defparameter *objects* (list *player* *npc*))


;;;; Config -------------------------------------------------------------------
(defun config ()
  (blt:set (format nil "window.size = ~Dx~D" *screen-width* *screen-height*))
  (blt:set "window.title = /r/roguelikedev")
  (blt:set "window.cellsize = 20x20")
  (blt:set "output.vsync = true")
  (blt:set "font: ./assets/ProggySquare/ProggySquare.ttf, size=20x20, spacing=1x1, align=dead-center;")
  (setf *running* t))


;;;; Rendering ----------------------------------------------------------------
(defun draw-objects ()
  (map nil #'draw-object *objects*))

(defun clear-objects ()
  (map nil #'clear-object *objects*))

(defun draw ()
  (draw-objects)
  (blt:refresh)
  (clear-objects))


;;;; Input --------------------------------------------------------------------
(defun event ()
  (if (blt:has-input-p)
    (blt:key-case (blt:read)
      (:up :move-up)
      (:down :move-down)
      (:left :move-left)
      (:right :move-right)
      (:escape :quit)
      (:close :quit))
    :done))

(defun handle-event (event)
  (ecase event
    (:move-up (move-object *player* 0 -1))
    (:move-down (move-object *player* 0 1))
    (:move-left (move-object *player* -1 0))
    (:move-right (move-object *player* 1 0))
    (:quit (setf *running* nil))))

(defun handle-events ()
  (iterate
    (for event = (event))
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
  (main))
