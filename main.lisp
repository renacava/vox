(in-package #:vox)

(defparameter *projection-matrix* nil)
(defparameter *my-chunk* nil)
(defparameter *my-chunk2* nil)
(defparameter *my-chunks* nil)
(defparameter *texture-atlas-tex* nil)
(defparameter *texture-atlas-sampler* nil)
(defparameter *rendering-paused?* nil)

(defmacro with-paused-rendering (&body body)
  `(let ((prior-state *rendering-paused?*))
     (setf *rendering-paused?* t)
     ,@body
     (setf *rendering-paused?* prior-state)))

(defun setup-lparallel-kernel (&optional (worker-threads 16))
  (unless lparallel:*kernel*
    (setf lparallel:*kernel* (lparallel:make-kernel worker-threads))))

(defun try-free (object)
  (when object (free object)))

(defun try-free-objects (&rest objects)
  (mapcar #'try-free objects))

(defstruct-g block-vert
  (pos :vec3)
  (uv :vec2))

(defun-g vert-stage ((block-vert block-vert)
                     &uniform
                     (now :float)
                     (proj :mat4)
                     (offset :vec3)
                     (chunk-width :int))
  (let* ((pos (vec4 (block-vert-pos block-vert) 1))
         (offset (* offset chunk-width))
         (pos (+ pos (vec4 offset 0)))
         ;;(pos (+ pos (vec4 (- (* 100 (sin now)) 95) (- (* 12 (cos now)) 8) -20 0)))
         (pos (+ pos (vec4 -127 (+ 70 (* 120 (sin now))) -170 0)))
         )
    (values (* proj pos)
            (block-vert-uv block-vert))))

(defun-g frag-stage ((uv :vec2) &uniform (atlas-sampler :sampler-2d))
  (texture atlas-sampler uv))

(defpipeline-g basic-pipeline ()
  (vert-stage block-vert)
  (frag-stage :vec2))

(defun now ()
  (float
   (/ (second (multiple-value-list
             (truncate (multiple-value-bind (seconds subseconds) 
                           (get-precise-time)
                         (+ (* seconds precise-time-units-per-second) subseconds))
                       (* precise-time-units-per-second 100000))))
      10000000)))

(defun make-chunks (radius &optional (width 8))
  (setup-lparallel-kernel)
  ;;(mapcar #'try-free *my-chunks*)
  ;;(loop for chunk in *my-chunks* do (ignore-errors (try-free chunk)))
  (setf *my-chunks* nil)
  (let* ((chunk-offsets (loop for i below radius
                              append (loop for j below radius
                                           append (loop for k below radius
                                                        collect (list j (truncate (- i)) (- k)))
                                           )))
         (offset-groups (group chunk-offsets 16)))
    (bt:make-thread
     (lambda ()
       (loop for offset-group in offset-groups
             do (lparallel:pmapcar (lambda (offset) (let ((mesh-data (make-chunk-mesh-data :width width)))
                                                      (queue-chunk mesh-data offset width)))
                                   offset-group))))
    )
  )

(defun setup-projection-matrix ()
  (setf *projection-matrix* (rtg-math.projection:perspective (x (resolution (current-viewport)))
                                                             (y (resolution (current-viewport)))
                                                             0.1
                                                             10000f0
                                                             60f0)))

(defun init (&optional (width 16) (radius 8))
  (setf (surface-title (current-surface)) "vox")
  (try-free-objects *texture-atlas-tex* *texture-atlas-sampler*)
  (setf *texture-atlas-tex* (or 
                             (ignore-errors (dirt:load-image-to-texture "texture-atlas.png"))
                             (ignore-errors (dirt:load-image-to-texture "projects/vox/texture-atlas.png"))))
  (setf *texture-atlas-sampler* (sample *texture-atlas-tex*
                                        :minify-filter :nearest-mipmap-nearest
                                        :magnify-filter :nearest))
  (setup-projection-matrix)
  
  (make-chunks radius width)
  )

(defparameter *delta* 1.0)
(defparameter *fps* 1)

(defun step-rendering ()
  (unless *rendering-paused?*
    (clear)

    (loop for chunk in *my-chunks*
          do (when chunk
               (unless (buffer-stream chunk)
                 (when (and (vert-array chunk) (index-array chunk))
                   (setf (buffer-stream chunk) (make-buffer-stream (vert-array chunk) :index-array (index-array chunk))))
                 )
               (map-g #'basic-pipeline (buffer-stream chunk)
                      :now (now)
                      :proj *projection-matrix*
                      :offset (offset chunk)
                      :chunk-width (width chunk)
                      :atlas-sampler *texture-atlas-sampler*)))
    
    (step-host)
    (swap)))

(defparameter ctx nil)
(defparameter dirty? nil)
(defparameter flipflop nil)
(defparameter queued-chunks nil)
(defparameter half-baked-chunks nil)

(defun queue-chunk (mesh-data offset width)
  (push (list mesh-data offset width) queued-chunks))

(defparameter inner-loader-thread-func (lambda ()
                                         (if queued-chunks
                                             (let* ((queued-chunk (pop queued-chunks))
                                                    (mesh-data (first queued-chunk))
                                                    (offset (v! (second queued-chunk)))
                                                    (width (third queued-chunk))
                                                    (chunk (ignore-errors
                                                            (make-instance 'chunk
                                                                           :width width
                                                                           :offset offset
                                                                           :vert-array (make-gpu-array (first mesh-data) :element-type 'block-vert)
                                                                           :index-array (make-gpu-array (second mesh-data) :element-type :uint)
                                                                           :buffer-stream nil))))
                                               (try-free-objects (first mesh-data) (second mesh-data))
                                               (push chunk *my-chunks*)
                                               (gl:finish))
                                             
                                             (sleep 0.001))
                                         
                                         ;; (if dirty?
                                         ;;     (progn
                                         ;;       (setf flipflop (not flipflop))
                                         ;;       (setf dirty? nil)

                                         ;;       (loop for )
                                               
                                         ;;       ;; (if my-second-array
                                         ;;       ;;     (progn
                                         ;;       ;;       (try-free my-second-array)
                                         ;;       ;;       (setf my-second-array nil))
                                                   
                                         ;;       ;;     (progn
                                         ;;       ;;       (setf my-second-array (make-gpu-array cube-2))
                                         ;;       ;;       (gl:finish)))
                                         ;;       )
                                             
                                         ;;     (sleep 0.1))
                                         ))

(defun init-ctx ()
  (setf ctx (or ctx (cepl.context:make-context-shared-with-current-context))))

(defun make-loader-thread ()
  (init-ctx)
  
  (bt:make-thread (lambda ()
                    (with-cepl-context (loader-context ctx)
                      (loop (livesupport:continuable (funcall inner-loader-thread-func)))))))




(defparameter main-loop-func (lambda ()
                               (livesupport:continuable
                                 (if *rendering-paused?*
                                     (progn
                                       (step-host)
                                       (livesupport:update-repl-link)
                                       (sleep 0.25))
                                     (let ((start-time (now)))
                                       (step-rendering)
                                       (step-host)
                                       (livesupport:update-repl-link)
                                       (setf *delta* (- (now) start-time))
                                       (setf *fps* (truncate (/ 1.0 (if (= *delta* 0)
                                                                        1.0
                                                                        *delta*))))))
                                 )))

(defun main ()
  (cepl:repl 720 480)
  (init)
  (make-loader-thread)
  (loop (funcall main-loop-func)))


