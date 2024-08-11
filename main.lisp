(in-package #:vox)

(defparameter *projection-matrix* nil)


(defparameter *my-chunk* nil)

(defun try-free (object)
  (when object (free object)))

(defun try-free-objects (&rest objects)
  (mapcar #'try-free objects))

(defun-g vert-stage ((vert :vec3)
                     &uniform
                     (now :float)
                     (proj :mat4)
                     (offset :vec3)
                     (chunk-width :int))
  (let* ((pos (vec4 vert 1))
         (offset (* offset chunk-width))
         (pos (+ pos (vec4 offset 0)))
         (pos (+ pos (vec4 (- (* 30 (sin now)) 15) (- (* 30 (cos now)) 15) -100 0))))
    (values (* proj pos)
            vert)))

(defun-g frag-stage ((col :vec3))
  (vec4 (mod (aref col 0) 2)
        (mod (aref col 1) 2)
        (mod (aref col 2) 2)
        1)
  ;; (let ((col (+ (mod col 1.0) (vec3 0.5 0.5 0.5))))
  ;;   (vec4 col 1))
  )

(defpipeline-g basic-pipeline ()
  (vert-stage :vec3)
  (frag-stage :vec3))

(defun now ()
  (float (/ (get-internal-real-time) 1000)))

(defparameter *rendering-paused?* nil)

(defun init (&optional (width 64))
  (setf *rendering-paused?* t)
  (setf (surface-title (current-surface)) "vox")
  (try-free *my-chunk*)
  (setf *my-chunk* (make-chunk :width width :offset (list 0 0 0)))
  (setf *projection-matrix* (rtg-math.projection:perspective (x (resolution (current-viewport)))
                                                             (y (resolution (current-viewport)))
                                                             0.1
                                                             300f0
                                                             60f0))
  (setf *rendering-paused?* nil))

(defun step-rendering ()
  (unless *rendering-paused?*
    (clear)
    (when *my-chunk*
      (map-g #'basic-pipeline (buffer-stream *my-chunk*)
             :now (now)
             :proj *projection-matrix*
             :offset (offset *my-chunk*)
             :chunk-width (width *my-chunk*)))
    
    (step-host)
    (swap)))


(defparameter main-loop-func (lambda ()
                               (livesupport:continuable
                                 (step-rendering)
                                 (step-host)
                                 (livesupport:update-repl-link)
                                 (sleep 0.025))
                               

                               ))

(defun main ()
  (cepl:repl 720 480)
  (init)
  (loop (funcall main-loop-func)))


