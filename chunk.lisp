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

(let ((tri-total 0))
  (defun render-chunks ()
    (setf tri-total 0)
    (let ((n-chunks-rendered 0))
      (maphash (lambda (offset entry)
                 (render (car entry)))
               *chunks-at-offsets-table*))
    tri-total)

  (defmethod render ((chunk chunk))
    (let ((buffer-stream (buffer-stream chunk)))
      (when (and buffer-stream
                 (< 0 (buffer-stream-length buffer-stream))))
      (let ((light-level (max (aref sky-colour 0)
                              (aref sky-colour 1)
                              (aref sky-colour 2)
                              0.1)))

        (incf tri-total (/ (first (gpu-array-dimensions (index-array chunk))) 3))
        (map-g #'chunk-pipeline buffer-stream
               :cam-pos camera-current-pos
               :cam-rot camera-current-rot
               :now *now*
               :proj *projection-matrix*
               :offset (offset chunk)
               :chunk-width (truncate (width chunk))
               :chunk-height (truncate (height chunk))
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
               )))))

(defparameter lod 0)

(defun make-chunks (radius-x &optional (width *chunk-width*) (height *chunk-height*) (radius-z radius-x))
  (setf chunks-queued-to-be-freed? t)
  (setf queued-primordial-chunks nil)
  (let* ((chunk-offsets (loop for i below radius-x
                              append (loop for j below radius-z
                                           collect (list j 0 (truncate i)))))
         ;;(offset-groups (group chunk-offsets 6))
         )
    (loop for offset in (reverse chunk-offsets)
          do (push (list offset width height) queued-primordial-chunks))))

(defun regen-chunk (&optional (chunk-offset `(0 0 0)) block-positions-and-symbols)
  (let ((chunk (gethash chunk-offset *chunks-at-offsets-table*))
        (blocks nil))
    (when chunk
      (setf chunk (car chunk))
      (setf blocks (or block-positions-and-symbols
                       (vox-world-sample:make-random-chunk-blocks2d (width chunk) (height chunk) chunk-offset lod)))
      (push (list chunk-offset (width chunk) (height chunk)) queued-primordial-chunks)
      (remhash chunk-offset *chunks-at-offsets-table*)
      (try-free chunk))))

(defun make-chunk (chunk-offset block-positions-and-symbols width height)
  "Block-positions-and-symbols should be a list of sublists where each sublist is (x y z block-symbol)."
  (declare (optimize (speed 3))
           (type fixnum width height))
  (when block-positions-and-symbols   
    (if (queue-full?)
        (progn (sleep 0.0001)
               (make-chunk chunk-offset block-positions-and-symbols width height))
        (queue-chunk (make-chunk-mesh-from-data block-positions-and-symbols chunk-offset width height)
                     chunk-offset
                     width
                     height))
    ;;(try-queue-chunk chunk-offset block-positions-and-symbols width height)
    ))

(defun try-queue-chunk (chunk-offset block-positions-and-symbols width height)
  (if (queue-full?)
      (progn (sleep 0.0001)
             (try-queue-chunk chunk-offset block-positions-and-symbols width height))
      (queue-chunk (make-chunk-mesh-from-data block-positions-and-symbols chunk-offset width height)
                   chunk-offset
                   width
                   height)))

(defun make-chunk-mesh-from-data (block-positions-and-symbols chunk-offset chunk-width chunk-height)
  "Returns the mesh-data for a chunk made of the given block-symbols at given block-positions."
  (declare (optimize (speed 3) (safety 0))
           (type fixnum chunk-width chunk-height))
  (combine-blocks-verts-and-indices
   (make-blocks-verts-and-indices-from-positions-and-symbols
    block-positions-and-symbols
    (first chunk-offset) (second chunk-offset) (third chunk-offset)
    chunk-width
    chunk-height)))

(defun queue-full? ()
  (< chunk-queue-max-size (length queued-chunks)))

(defun queue-chunk (mesh-data offset width height)
  (push (list mesh-data offset width height) queued-chunks))
