(in-package :rl)

(defvar *running* t)

(defun config ()
  (setf *running* t))

(defun draw ()
  (blt:clear)
  (blt:print 0 0 "Hello, r/roguelikedev!")
  (blt:refresh))

(defun event ()
  (if (blt:has-input-p)
    (blt:key-case (blt:read)
      (:escape :quit)
      (:close :quit))
    :done))

(defun handle-event (event)
  (ecase event
    (:quit (setf *running* nil))))

(defun handle-events ()
  (iterate
    (for event = (event))
    (until (eql event :done))
    (when event
      (handle-event event))))

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
