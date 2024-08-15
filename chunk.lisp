(in-package #:vox)

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
          :initform 2)))

(defun make-chunk (&key (width 2) (offset (list 0 0 0)))
  (let ((mesh-data (make-chunk-buffer-stream :width width :height width :depth width)))
    (make-instance 'chunk
                   :width width
                   :offset (vec3 (float (first offset))
                                 (float (second offset))
                                 (float (third offset)))
                   :vert-array (first mesh-data)
                   :index-array (second mesh-data)
                   :buffer-stream (third mesh-data))))

(defmethod free ((chunk chunk))
  (setq current-chunk chunk)
  (try-free-objects
   (buffer-stream chunk)
   (index-array chunk)
   (vert-array chunk)))

(defun make-chunk-buffer-stream (&key (width 2) (height 2) (depth 2))
  "Returns a buffer-stream object for the mesh of a chunk of the given dimensions, made of cubes."
  (let* ((block-indices (make-chunk-block-indices :width width :height height :depth depth))
         (blocks-verts-and-indices (make-blocks-verts-and-indices block-indices))
         (mesh-data (combine-blocks-verts-and-indices blocks-verts-and-indices))
         (verts-gpu-array (make-gpu-array (first mesh-data)  :element-type :vec2))
         (indices-gpu-array (make-gpu-array (second mesh-data) :element-type :uint)))
    (list verts-gpu-array
          indices-gpu-array
          (make-buffer-stream verts-gpu-array :index-array indices-gpu-array))))

(defun make-chunk-mesh-data (&key (width 2) (height width) (depth width))
  "Returns the mesh-data for a chunk of the given dimensions."
  (let* ((block-indices (make-chunk-block-indices :width width :height height :depth depth))
         (blocks-verts-and-indices (make-blocks-verts-and-indices block-indices)))
    (combine-blocks-verts-and-indices blocks-verts-and-indices)
    ))

(defun make-chunk-buffer-stream-from-mesh-data (mesh-data)
  "Returns a buffer-stream object for a chunk based off of mesh-data."
  (let* ((verts-gpu-array (make-gpu-array (first mesh-data)))
         (indices-gpu-array (make-gpu-array (second mesh-data) :element-type :uint)))
    (list verts-gpu-array
          indices-gpu-array
          (make-buffer-stream verts-gpu-array :index-array indices-gpu-array))))

(defun make-chunk-block-indices (&key (width 2) (height 2) (depth 2))
  (loop for indices in
        (loop for x below width
              append (loop for y below height
                           append (loop for z below depth
                                        collect (make-array 3 :element-type 'fixnum  :initial-contents (vector x y z)))))
        collect (let ((result
                        ;; (if (and (evenp (first indices))
                        ;;          (evenp (second indices))
                        ;;          (evenp (third indices)))
                        ;;     indices
                        ;;     nil)
                        (indices-on-chunk-border-p indices width)))
                  (when result (vec3-to-index result)))
        ))

(defun indices-on-chunk-border-p (indices chunk-width)
  "Returns indices if on the border of the chunk, assuming cubic chunk, else nil."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum chunk-width))
  (declare (type (simple-array fixnum) indices))

  (let ((x (aref indices 0))
        (y (aref indices 1))
        (z (aref indices 2))
        (border (1- chunk-width)))
    (declare (type fixnum x y z border))
    (if (or (= x 0)
            (= y 0)
            (= z 0)
            (= z border)
            (= y border)
            (= x border))
        indices
        nil)))
  
