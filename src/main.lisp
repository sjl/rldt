(in-package :rl)

;;;; Parameters ---------------------------------------------------------------
(defvar *running* t)

(defparameter *screen-width* 80)
(defparameter *screen-height* 40)

(defparameter *player-x* (truncate *screen-width* 2))
(defparameter *player-y* (truncate *screen-height* 2))

(defparameter *layer-bg* 0)
(defparameter *layer-mobs* 1)


;;;; Config -------------------------------------------------------------------
(defun config ()
  (blt:set (format nil "window.size = ~Dx~D" *screen-width* *screen-height*))
  (blt:set "window.title = /r/roguelikedev")
  (blt:set "window.cellsize = 20x20")
  (blt:set "output.vsync = true")
  (blt:set "font: ./assets/ProggySquare/ProggySquare.ttf, size=20x20, spacing=1x1, align=dead-center;")
  (setf *running* t))


;;;; Rendering ----------------------------------------------------------------
(defun draw-player ()
  (setf (blt:layer) *layer-mobs*)
  (blt:print *player-x* *player-y* "@"))

(defun clear-player ()
  (setf (blt:layer) *layer-mobs*)
  (blt:print *player-x* *player-y* " "))

(defun draw ()
  (draw-player)
  (blt:refresh)
  (clear-player))


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
    (:move-up (decf *player-y*))
    (:move-down (incf *player-y*))
    (:move-left (decf *player-x*))
    (:move-right (incf *player-x*))
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
