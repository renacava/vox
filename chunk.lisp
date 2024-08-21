(in-package #:vox)

(defun make-random-chunk-blocks (&optional (width *chunk-width*) (height *chunk-height*))
  (loop for x below width
        append (loop for z below width
                     append (loop for y below height
                                  ;;when (> 1 (random 3))
                                  collect (list x y z
                                                (cond ((< y 4) 'bricks)
                                                      ((< y 16) 'cobblestone)
                                                      ((< y 20) 'grass)
                                                      (t nil)))))))

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
  (remhash (coerce (offset chunk) 'list) *chunks-at-offsets-table*)
  (let* ((vert-array (vert-array chunk))
         (index-array (index-array chunk))
         (buffer-stream (buffer-stream chunk)))
    (try-free-objects buffer-stream index-array vert-array)
    (setf chunk nil)))

(defun make-chunks (radius &optional (width *chunk-width*) (height *chunk-height*))
  (setup-lparallel-kernel)
  (setf chunks-queued-to-be-freed? t)
  (let* ((chunk-offsets (loop for i below radius
                              append (loop for j below radius
                                           collect (list j 0 (truncate i)))))
         (offset-groups (group chunk-offsets 6)))
    (bt:make-thread
     (lambda ()
       (loop for offset-group in offset-groups
             do (lparallel:pmapcar (lambda (offset)
                                     (make-chunk offset
                                                 (make-random-chunk-blocks)
                                                 width
                                                 height))
                                   offset-group))))))

(defun make-chunk (chunk-offset block-positions-and-symbols &optional (width *chunk-width*) (height *chunk-height*))
  "Block-positions-and-symbols should be a list of sublists where each sublist is (x y z block-symbol)."
  (labels ((queue ()
             (if (queue-full?)
                 (progn (sleep 0.0001)
                        (queue))
                 (queue-chunk (make-chunk-mesh-from-data block-positions-and-symbols)
                              chunk-offset
                              width
                              height))))
    (queue)))

(defun make-chunk-mesh-from-data (block-positions-and-symbols)
  "Returns the mesh-data for a chunk made of the given block-symbols at given block-positions."
  (combine-blocks-verts-and-indices
   (make-blocks-verts-and-indices-from-positions-and-symbols
    (remove-if-not (lambda (pos-and-symb) (last1 pos-and-symb)) block-positions-and-symbols))))
