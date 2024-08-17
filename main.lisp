(in-package #:vox)

(defparameter *projection-matrix* nil)
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
  (texture-atlas-column :float)
  (texture-atlas-row :float)
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

(defun-g atlas-column-row-to-uv-offset ((column :float) (row :float) (atlas-size :int))
  (vec2 (/ column atlas-size)
        (/ row atlas-size)))

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
         (pos (+ pos (vec4 offset 0)))
         (pos (* pos 0.5))
         (now (* 6 now))
         (pos (+ pos (vec4 (- (* 3 (sin now)) 2) (- (* 2 (cos now)) 3) -10)))
         (uv (/ (block-vert-uv vert) 2))
         (uv (+ uv (atlas-column-row-to-uv-offset
                    (block-vert-texture-atlas-column vert)
                    (block-vert-texture-atlas-row vert)
                    atlas-size))))
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



(defun setup-projection-matrix ()
  (setf *projection-matrix* (rtg-math.projection:perspective (x (resolution (current-viewport)))
                                                             (y (resolution (current-viewport)))
                                                             0.1
                                                             10000f0
                                                             60f0)))

(defun init (&optional (width *chunk-width*) (radius 8))
  (setf (surface-title (current-surface)) "vox")
  (try-free-objects *texture-atlas-tex* *texture-atlas-sampler*)
  (setf *texture-atlas-tex* (or 
                             (ignore-errors (dirt:load-image-to-texture "texture-atlas.png"))
                             (ignore-errors (dirt:load-image-to-texture "projects/vox/texture-atlas.png"))))
  (setf *texture-atlas-sampler* (sample *texture-atlas-tex*
                                        :minify-filter :nearest-mipmap-nearest
                                        :magnify-filter :nearest))
  
  (make-chunks radius width))

(defparameter *delta* 1.0)
(defparameter *fps* 1)

(defun step-rendering ()
  (unless *rendering-paused?*
    (clear)
    (setup-projection-matrix)
    (maphash (lambda (offset chunk)
               (when chunk
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
             *chunks-at-offsets-table*)
    
    (step-host)
    (swap)))

(defparameter ctx nil)
(defparameter dirty? nil)
(defparameter flipflop nil)
(defparameter queued-chunks nil)
(defparameter chunk-queue-max-size 256)
(defparameter half-baked-chunks nil)
(defparameter chunks-queued-to-be-freed? nil)
(defparameter *chunks-at-offsets-table* (make-hash-table :test #'equal))

(defun queue-full? ()
  (< chunk-queue-max-size (length queued-chunks)))

(defun queue-chunk (mesh-data offset width)
  (push (list mesh-data offset width) queued-chunks))

(defparameter errors nil)

(defparameter inner-loader-thread-func (lambda ()
                                         (if chunks-queued-to-be-freed?
                                             (with-paused-rendering
                                               (maphash (lambda (offset chunk)
                                                          (ignore-errors (try-free chunk)))
                                                        *chunks-at-offsets-table*)
                                               (clrhash *chunks-at-offsets-table*)
                                               (setf chunks-queued-to-be-freed? nil)))
                                         
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
                                                   (let* ((offset (second queued-chunk))
                                                          (existing-chunk (gethash offset *chunks-at-offsets-table*)))
                                                     (when existing-chunk (try-free existing-chunk))
                                                     (setf (gethash offset *chunks-at-offsets-table*) chunk))
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
                                                                        *delta*)))))))))

(defun main ()
  (cepl:repl 720 480)
  (init)
  (make-loader-thread)
  (loop (funcall main-loop-func)))


