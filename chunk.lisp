(in-package #:vox)

(defparameter queued-primordial-chunks nil)
(defparameter queued-chunks nil)
(defparameter chunk-queue-max-size 16)
(defparameter half-baked-chunks nil)
(defparameter chunks-queued-to-be-freed? nil)
(defparameter *chunks-at-offsets-table* (make-hash-table :test #'equal))
(defparameter chunk-table-lock (bt:make-lock "chunk-table-lock"))

(defclass chunk ()
  ((buffer-stream :initarg :buffer-stream
                  :accessor buffer-stream
                  :initform nil)
   (vert-array :initarg :vert-array
               :accessor vert-array
               :initform nil)
   (index-array :initarg :index-array
                :accessor index-array
                :initform nil)
   (offset :initarg :offset
           :accessor offset
           :initform (list 0 0 0))
   (width :initarg :width
          :accessor width
          :initform *chunk-width*)
   (height :initarg :height
           :accessor height
           :initform *chunk-height*)))

(defmethod free ((chunk chunk))
  (bt:with-lock-held (chunk-table-lock)
    (remhash (coerce (offset chunk) 'list) *chunks-at-offsets-table*))
  (let* ((vert-array (vert-array chunk))
         (index-array (index-array chunk))
         (buffer-stream (buffer-stream chunk)))
    (try-free-objects buffer-stream index-array vert-array)
    (setf chunk nil)))

(defmethod render ((things-to-render list))
  (mapcar #'render things-to-render))

(defmethod render ((chunk chunk))
  (let ((buffer-stream (buffer-stream chunk)))
    (when (and buffer-stream
               (< 0 (buffer-stream-length buffer-stream))))
    (let ((light-level (max (aref sky-colour 0)
                            (aref sky-colour 1)
                            (aref sky-colour 2)
                            0.1)))

      (map-g #'chunk-pipeline buffer-stream
             :cam-pos camera-current-pos
             :cam-rot camera-current-rot
             :now *now*
             :proj *projection-matrix*
             :offset (offset chunk)
             :chunk-width (truncate *chunk-width*)
             :chunk-height (truncate *chunk-height*)
             :texture-atlas-ssbo texture-atlas-ssbo
             :atlas-size *texture-atlas-size*
             :skylight-colour (lerp-vec3
                               (vec3 (aref sky-colour 0)
                                     (aref sky-colour 1)
                                     (aref sky-colour 2))
                               (vec3 light-level light-level light-level)
                               0.9)
             :sky-colour sky-colour
             :lod (float lod)
             )
      )))
(defparameter lod 0f0)
;;(defparameter chunks-per-step-host 200)

(defun render-chunks ()
  (let ((n-chunks-rendered 0))
    (maphash (lambda (offset entry)
               (render (car entry)))
             *chunks-at-offsets-table*)
    
    ))

(defun make-chunks (radius-x &optional (width *chunk-width*) (height *chunk-height*) (radius-z radius-x))
  ;;(setup-lparallel-kernel)
  (setf chunks-queued-to-be-freed? t)
  (setf queued-primordial-chunks nil)
  (let* ((chunk-offsets (loop for i below radius-x
                              append (loop for j below radius-z
                                           collect (list j 0 (truncate i)))))
         (offset-groups (group chunk-offsets 6)))
    (loop for offset in (reverse chunk-offsets)
          do (push offset queued-primordial-chunks))
    ;; (bt:make-thread
    ;;  (lambda ()
    ;;    (loop for offset-group in offset-groups
    ;;          do (ignore-errors
    ;;              (lparallel:pmapcar (lambda (offset)
    ;;                                   (make-chunk offset
    ;;                                               ;;(vws:make-random-chunk-blocks3d offset)
    ;;                                               (vox-world-sample:make-random-chunk-blocks2d offset)
    ;;                                               ;;(vox-world-sample:make-random-chunk-blocks2d-columns offset)
    ;;                                               ;;(vox-world-sample::make-random-chunk-blocks-caved offset)
    ;;                                               ;;(vox-world-sample:make-slicey-chunk offset)
    ;;                                               width
    ;;                                               height))
    ;;                                 offset-group)))))
    ))

(defun regen-chunk (&optional (chunk-offset `(0 0 0)) (block-positions-and-symbols (vox-world-sample:make-random-chunk-blocks2d chunk-offset)))
  (let ((chunk (gethash chunk-offset *chunks-at-offsets-table*)))
    (when chunk
      (remhash chunk-offset *chunks-at-offsets-table*)
      (try-free (car chunk))))
  (push chunk-offset queued-primordial-chunks)
  ;;(vox::make-chunk chunk-offset block-positions-and-symbols)
  )

(defun make-chunk (chunk-offset block-positions-and-symbols &optional (width *chunk-width*) (height *chunk-height*))
  "Block-positions-and-symbols should be a list of sublists where each sublist is (x y z block-symbol)."
  (when block-positions-and-symbols
    (labels ((queue ()
               (if (queue-full?)
                   (progn (sleep 0.0001)
                          (queue))
                   (queue-chunk (make-chunk-mesh-from-data block-positions-and-symbols chunk-offset)
                                chunk-offset
                                width
                                height))))
      (queue))
    ;; (queue-chunk (make-chunk-mesh-from-data block-positions-and-symbols)
    ;;              chunk-offset
    ;;              width
    ;;              height)
    ))

(defun make-chunk-mesh-from-data (block-positions-and-symbols chunk-offset)
  "Returns the mesh-data for a chunk made of the given block-symbols at given block-positions."
  (combine-blocks-verts-and-indices
   (make-blocks-verts-and-indices-from-positions-and-symbols
    (remove-if-not (lambda (pos-and-symb) (last1 pos-and-symb)) block-positions-and-symbols)
    chunk-offset)))

(defun queue-full? ()
  (< chunk-queue-max-size (length queued-chunks)))

(defun queue-chunk (mesh-data offset width height)
  (push (list mesh-data offset width height) queued-chunks)
  )
