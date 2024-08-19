(in-package #:vox)

(defparameter *chunk-width* 16)
(defparameter *default-chunk* (loop for x below 8
                                    append (loop for y below 8
                                                 append (loop for z below 8
                                                              when (> 1 (random 3))
                                                              collect (list x y z (elt-random `(grass bricks cobblestone gobbledygook-nonsense)))))))


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


(defmethod free ((chunk chunk))*
  (remhash (coerce (offset chunk) 'list) *chunks-at-offsets-table*)
  (let* ((vert-array (vert-array chunk))
         (index-array (index-array chunk))
         (buffer-stream (buffer-stream chunk)))
    (try-free-objects buffer-stream index-array vert-array)
    (setf chunk nil)))

(defun make-chunks (radius &optional (width *chunk-width*))
  (setup-lparallel-kernel)
  (setf chunks-queued-to-be-freed? t)
  (let* ((chunk-offsets (loop for i below radius
                              append (loop for j below radius
                                           append (loop for k below radius
                                                        collect (list j (truncate i) (- k)))
                                           )))
         (offset-groups (group chunk-offsets 6)))
    (bt:make-thread
     (lambda ()
       (loop for offset-group in offset-groups
             do (lparallel:pmapcar (lambda (offset)
                                     (make-chunk offset ;; (list (list 0 0 0 'grass)
                                                 ;;       (list 0 1 0 'cobblestone)
                                                 ;;       (list 0 1 1 'bricks))
                                                 (make-random-chunk-blocks)
                                                 width))
                                   offset-group))))))

(defun make-chunk (chunk-offset block-positions-and-symbols &optional (width *chunk-width*))
  "Block-positions-and-symbols should be a list of sublists where each sublist is (x y z block-symbol)."
  (labels ((queue ()
             (if (queue-full?)
                 (progn (sleep 0.0001)
                        (queue))
                 (queue-chunk (make-chunk-mesh-from-data block-positions-and-symbols width) chunk-offset width))))
    (queue)))

(defun make-chunk-mesh-from-data (block-positions-and-symbols &optional (width *chunk-width*))
  "Returns the mesh-data for a chunk made of the given block-symbols at given block-positions."
  (let* ((blocks-verts-and-indices (make-blocks-verts-and-indices-from-positions-and-symbols block-positions-and-symbols)))
    (combine-blocks-verts-and-indices blocks-verts-and-indices)))

(defun make-chunk-mesh-data (&key (width 2) (height width) (depth width))
  "Returns the mesh-data for a chunk of the given dimensions."
  (let* ((block-indices (make-chunk-block-indices :width width :height height :depth depth))
         (blocks-verts-and-indices (make-blocks-verts-and-indices block-indices)))
    (combine-blocks-verts-and-indices blocks-verts-and-indices)))

(defun make-chunk-block-indices (&key (width 2) (height 2) (depth 2))
  (loop for indices in
        (loop for x below width
              append (loop for y below height
                           append (loop for z below depth
                                        collect (vec3 (float x) (float y) (float z)))))
        collect (if (and (evenp (truncate (aref indices 0)))
                         (evenp (truncate (aref indices 1)))
                         (evenp (truncate (aref indices 2))))
                    indices
                    nil)
        ;;(indices-on-chunk-border-p indices width)
        ))

(defun indices-on-chunk-border-p (indices chunk-width)
  "Returns indices if on the border of the chunk, assuming cubic chunk, else nil."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum chunk-width))
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
  
