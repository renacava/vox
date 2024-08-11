(in-package #:vox)

(defparameter *projection-matrix* (rtg-math.projection:perspective (x (resolution (current-viewport)))
                                                                   (y (resolution (current-viewport)))
                                                                   0.1
                                                                   30f0
                                                                   60f0))


(defparameter *my-chunk* nil)

(defun try-free (object)
  (when object (free object)))

(defun try-free-objects (&rest objects)
  (mapcar #'try-free objects))

(defun-g vert-stage ((vert :vec3)
                     &uniform
                     (now :float)
                     (proj :mat4)
                     (rot :vec3))
  (let* ((pos (* (rtg-math.matrix4:rotation-from-euler rot) (vec4 vert 1)))
         (pos (+ pos (vec4 (* 2 (sin now)) (* 3 (cos now)) -5 0))))
    (values (* proj pos)
            vert)))

(defun-g frag-stage ((col :vec3))
  (let ((col (+ col (vec3 0.5 0.5 0.5))))
    (vec4 col 1)))

(defpipeline-g basic-pipeline ()
  (vert-stage :vec3)
  (frag-stage :vec3))

(defun now ()
  (float (/ (get-internal-real-time) 1000)))

(defun init (&optional (width 4))
  (try-free *my-chunk*)
  (setf *my-chunk* (make-chunk :width width :offset (list 1 0 0))))

(defun step-rendering ()
  (clear)
  (when *my-chunk*
    (map-g #'basic-pipeline (buffer-stream *my-chunk*)
           :now (now)
           :proj *projection-matrix*
           :rot (v! (* 90 0.03 (now)) (* 90 0.02 (now)) (* 90 0.01 (now)))))
  
  (step-host)
  (swap))


(defparameter main-loop-func (lambda ()
                               ;;(step-rendering)
                               (step-host)
                               (sleep 0.025)
                               ))

(defun main ()
  (cepl:repl)
  (init)
  (loop (funcall main-loop-func)))


