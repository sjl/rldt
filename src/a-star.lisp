(in-package :rl.a*)



(defstruct path
  state
  estimate
  (cost 0)
  (previous nil))

(defun path-to-list (path)
  (reverse (recursively ((path path))
             (unless (null path)
               (cons (path-state path)
                     (recur (path-previous path)))))))

(defmethod print-object ((path path) s)
  (format s "#<PATH ~A cost ~A/~A>"
          (path-to-list path)
          (path-cost path)
          (path-estimate path)))


(defun a* (&key start neighbors goal-p cost heuristic (test 'eql))
  (iterate
    (with seen = (make-hash-table :test test))
    (with frontier = (pileup:make-heap #'< :key #'path-estimate))

    (initially (pileup:heap-insert (make-path :state start :estimate 0) frontier)
               (setf (gethash start seen) 0))

    (for (values current found) = (pileup:heap-pop frontier))
    (unless found
      (return (values nil nil)))

    (for current-state = (path-state current))

    (when (funcall goal-p current-state)
      (return (values (path-to-list current) t)))

    (for current-cost = (path-cost current))

    (iterate
      (for next-state :in (funcall neighbors current-state))
      (for next-cost = (+ current-cost (funcall cost current-state next-state)))
      (for (values seen-cost previously-seen) = (gethash next-state seen))
      (when (or (not previously-seen)
                (< next-cost seen-cost))
        (for next-estimate = (+ next-cost (funcall heuristic next-state)))
        (for next = (make-path :state next-state
                               :cost next-cost
                               :estimate next-estimate
                               :previous current))
        (setf (gethash next-state seen) next-cost)
        (pileup:heap-insert next frontier)))))
