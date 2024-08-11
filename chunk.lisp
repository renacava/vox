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
                   :offset offset
                   :vert-array (first mesh-data)
                   :index-array (second mesh-data)
                   :buffer-stream (third mesh-data))))

(defmethod free ((chunk chunk))
  (try-free-objects (vert-array chunk)
                    (index-array chunk)
                    (buffer-stream chunk)))

(defun make-chunk-buffer-stream (&key (width 2) (height 2) (depth 2))
  "Returns a buffer-stream object for the mesh of a chunk of the given dimensions, made of cubes."
  (let* ((block-indices (make-chunk-block-indices :width width :height height :depth depth))
         (blocks-verts-and-indices (make-blocks-verts-and-indices block-indices))
         (mesh-data (combine-blocks-verts-and-indices blocks-verts-and-indices))
         (verts-gpu-array (make-gpu-array (first mesh-data)))
         (indices-gpu-array (make-gpu-array (second mesh-data) :element-type :uint)))
    (list verts-gpu-array
          indices-gpu-array
          (make-buffer-stream verts-gpu-array :index-array indices-gpu-array))))

(defun make-chunk-block-indices (&key (width 2) (height 2) (depth 2))
  (loop for x below width
        append (loop for y below height
                     append (loop for z below depth
                                  collect (list x y z)))))
