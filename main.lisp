(in-package #:vox)

(defparameter *projection-matrix* nil)
(defparameter *my-chunk* nil)
(defparameter *my-chunk2* nil)
(defparameter *my-chunks* nil)
(defparameter *texture-atlas-tex* nil)
(defparameter *texture-atlas-sampler* nil)
(defparameter *texture-atlas-size* 2)
(defparameter *rendering-paused?* nil)

(defmacro with-paused-rendering (&body body)
  `(let ((prior-state *rendering-paused?*))
     (setf *rendering-paused?* t)
     (unwind-protect
          (progn
            ,@body)
       (setf *rendering-paused?* prior-state))))

(defun setup-lparallel-kernel (&optional (worker-threads 16))
  (unless lparallel:*kernel*
    (setf lparallel:*kernel* (lparallel:make-kernel worker-threads))))

(defun try-free (object)
  (when object (free object)))

(defun try-free-objects (&rest objects)
  (mapcar #'try-free objects))

(defstruct-g block-vert
  (vert :vec3)
  (uv :vec2)
  (id :float)
  (local-offset :vec3))

(defun-g index-to-xyz-g ((index :float))
  (vec3 (int (mod index 64))
        (int (mod (/ index 64) 64))
        (int (mod (/ (/ index 64) 64) 64))))

(defun-g index-to-xy-g ((index :float))
  (vec2 (int (mod index 64))
        (int (mod (/ index 64) 64))))

(defun-g ivec2-to-vec2 ((ivec2 :ivec2))
  (vec2 (aref ivec2 0) (aref ivec2 1)))

(defun-g ivec3-to-vec3 ((ivec3 :ivec3))
  (vec3 (aref ivec3 0) (aref ivec3 1) (aref ivec3 2)))

(defun-g id-to-uv-offset ((id :int) (atlas-size :int))
  (vec2 (/ (float (mod id atlas-size)) atlas-size)
        (/ (float (/ id atlas-size)) atlas-size)))

(defun-g vert-stage ((vert block-vert)
                     &uniform
                     (now :float)
                     (proj :mat4)
                     (offset :vec3)
                     (chunk-width :int)
                     (atlas-size :int))
  (let* ((pos (block-vert-vert vert))
         (pos (+ pos (block-vert-local-offset vert)))
         (pos (vec4 pos 1))
         (offset (* offset chunk-width))
         (offset (+ offset (* 0.2 (+ 1 (cos (* (* 112 (* 0.3 (block-vert-id vert))) now))))))
         (pos (+ pos (vec4 offset 0)))
         (pos (* pos 0.5))
         (pos (+ pos (vec4 (- (* 25 (sin now)) 25) (- (* 12 (cos now)) -10) -200;;(* -10 (+ 2 (sin now))) 0
                           )))
         (uv (/ (block-vert-uv vert) 2))
         (id (int (block-vert-id vert)))
         (uv (+ uv (id-to-uv-offset id atlas-size)))
         )
    (values (* proj pos)
            uv)))


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
      60000000)))

(defun make-chunks (radius &optional (width 8))
  (setup-lparallel-kernel)
  (setf chunks-queued-to-be-freed? t)
  (let* ((chunk-offsets (loop for i below radius
                              append (loop for j below radius
                                           append (loop for k below radius
                                                        collect (list j (truncate (- i)) (- k)))
                                           )))
         (offset-groups (group chunk-offsets 6)))
    (bt:make-thread
     (lambda ()
       (loop for offset-group in offset-groups
             do (lparallel:pmapcar (lambda (offset) (labels ((queue ()
                                                               (if (queue-full?)
                                                                   (progn (sleep 0.0001)
                                                                          (queue))
                                                                   (queue-chunk (make-chunk-mesh-data :width width) offset width))))
                                                      (queue)))
                                   offset-group))))))

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
  ;;(setup-projection-matrix)
  
  (make-chunks radius width))

(defparameter *delta* 1.0)
(defparameter *fps* 1)

(defun step-rendering ()
  (unless *rendering-paused?*
    (clear)
    (setup-projection-matrix)
    (loop for chunk in *my-chunks*
          do (when chunk
               (unless (buffer-stream chunk)
                 (when (and (vert-array chunk) (index-array chunk))
                   (setf (buffer-stream chunk) (make-buffer-stream (vert-array chunk) :index-array (index-array chunk)))))
               (unless *rendering-paused?*
                 (map-g #'basic-pipeline (buffer-stream chunk)
                       :now (now)
                       :proj *projection-matrix*
                       :offset (offset chunk)
                       :chunk-width (width chunk)
                       :atlas-sampler *texture-atlas-sampler*
                       :atlas-size *texture-atlas-size*))))
    
    (step-host)
    (swap)))

(defparameter ctx nil)
(defparameter dirty? nil)
(defparameter flipflop nil)
(defparameter queued-chunks nil)
(defparameter chunk-queue-max-size 256)
(defparameter half-baked-chunks nil)
(defparameter chunks-queued-to-be-freed? nil)

(defun queue-full? ()
  (< chunk-queue-max-size (length queued-chunks)))

(defun queue-chunk (mesh-data offset width)
  (push (list mesh-data offset width) queued-chunks))

(defparameter errors nil)

(defparameter inner-loader-thread-func (lambda ()
                                         (if chunks-queued-to-be-freed?
                                             (with-paused-rendering
                                               (let ((freed-chunks 0)) 
                                                (loop for chunk in *my-chunks* do (when (ignore-errors (try-free chunk) t) (incf freed-chunks)))
                                                (setq n-freed-chunks (format nil "~a/~a" freed-chunks (length *my-chunks*)))
                                                (setf *my-chunks* nil)
                                                 (setf chunks-queued-to-be-freed? nil))))
                                         (if queued-chunks
                                             (let* ((queued-chunk (pop queued-chunks))
                                                    (mesh-data (first queued-chunk))
                                                    (offset (v! (second queued-chunk)))
                                                    (width (third queued-chunk))
                                                    (vert-array (ignore-errors (make-gpu-array (first mesh-data))))
                                                    (index-array (ignore-errors (make-gpu-array (second mesh-data) :element-type :uint)))
                                                    (chunk (when (and vert-array index-array)
                                                            (make-instance 'chunk
                                                                           :width width
                                                                           :offset offset
                                                                           :vert-array vert-array
                                                                           :index-array index-array
                                                                           :buffer-stream nil))))
                                               (try-free-objects (first mesh-data) (second mesh-data))
                                               (if chunk
                                                   (push chunk *my-chunks*)
                                                   (try-free-objects vert-array index-array))
                                               (gl:finish))
                                             
                                             (sleep 0.001))))

(defun init-ctx ()
  (setf ctx (or ctx (cepl.context:make-context-shared-with-current-context))))

(defun make-loader-thread ()
  (init-ctx)
  
  (bt:make-thread (lambda ()
                    (with-cepl-context (loader-context ctx)
                      (loop (livesupport:continuable (funcall inner-loader-thread-func)))))))

(defun get-cepl-context-surface-resolution ()
  (surface-resolution (current-surface (cepl-context))))

(defparameter main-loop-func (lambda ()
                               (livesupport:continuable
                                 (if *rendering-paused?*
                                     (progn
                                       (step-host)
                                       (livesupport:update-repl-link)
                                       (sleep 0.25))
                                     (let ((start-time (now)))
                                       (setf (resolution (current-viewport))
                                             (get-cepl-context-surface-resolution))
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


