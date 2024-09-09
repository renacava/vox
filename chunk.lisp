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
         ;;(index-array (index-array chunk))
         ;;(buffer-stream (buffer-stream chunk))
         )
    ;;(try-free-objects buffer-stream index-array vert-array)
    (try-free vert-array)
    (setf chunk nil)))

(defmethod render ((things-to-render list))
  (mapcar #'render things-to-render))

(defparameter *shared-gpu-arrays* nil)
(defparameter *shared-gpu-arrays-length* 0)
(defparameter *shared-gpu-arrays-outdated?* nil)
(defparameter *chunks-buffer* nil)

(defun rebuffer-chunk-gpu-arrays ()
  (mapcar #'try-free *shared-gpu-arrays*)
  (try-free *chunks-buffer*)
  (when (and (boundp 'my-cool-gpu-buffer) my-cool-gpu-buffer)
    (try-free my-cool-gpu-buffer))
  (let* ((c-arrays (all-chunk-vert-arrays)))
    (when c-arrays
      (setq my-cool-gpu-buffer (make-gpu-buffer :initial-contents c-arrays))
      (setf *shared-gpu-arrays* (make-gpu-arrays c-arrays)
            *shared-gpu-arrays-length* (c-arrays-total-length c-arrays)
            *chunks-buffer* (make-buffer-stream *shared-gpu-arrays* :length *shared-gpu-arrays-length*)
            )))
  ;;(setf *shared-gpu-arrays-outdated?* nil)
  )


(let ((tri-total 0)
      ;;(chunks-buffer)
      (gate t))
  (defun render-chunks ()
    (setf tri-total 0)
    ;;(when *shared-gpu-arrays-outdated?* (rebuffer-chunk-gpu-arrays))
    (let* ()
      ;; (when gate
      ;;   (setq my-cool-buffer (make-buffer-stream my-cool-gpu-buffer :length *shared-gpu-arrays-length*)))
      (when (and *chunks-buffer* (< 0 (buffer-stream-length *chunks-buffer*)))
        (map-g #'chunk-pipeline *chunks-buffer*
               :cam-pos camera-current-pos
               :cam-rot camera-current-rot
               :now *now*
               :proj *projection-matrix*
               :chunk-width 16
               :chunk-height 128
               :texture-atlas-ssbo texture-atlas-ssbo
               :atlas-size *texture-atlas-size*
               :skylight-colour skylight-colour
               :sky-colour sky-colour
               :lod (float lod)
               ))
      (incf tri-total (/ *shared-gpu-arrays-length* 3))
      )
    
    ;; (let* ((chunk1 (car (gethash `(0 0 0) *chunks-at-offsets-table*)))
    ;;        (chunk2 (car (gethash `(1 0 0) *chunks-at-offsets-table*)))
    ;;        (verts1 (vert-array chunk1))
    ;;        (verts2 (vert-array chunk2))
    ;;        (v1-length (first (gpu-array-dimensions verts1)))
    ;;        (v2-length (first (gpu-array-dimensions verts2)))
    ;;        (total-length (+ v1-length v2-length)))

      
    ;;   (when gate
    ;;     (setq my-c-arrays (list (pull1-g verts1)
    ;;                             (pull1-g verts2)))
    ;;     (setq my-gpu-arrays (make-gpu-arrays my-c-arrays))
    ;;     (setq my-cool-buffer (make-buffer-stream my-gpu-arrays :length total-length))
    ;;     (setf gate nil))
      
    ;;   (incf tri-total (/ (first (gpu-array-dimensions (index-array chunk1))) 3))
    ;;   (map-g #'chunk-pipeline my-cool-buffer
    ;;          :cam-pos camera-current-pos
    ;;          :cam-rot camera-current-rot
    ;;          :now *now*
    ;;          :proj *projection-matrix*
    ;;          :chunk-width (truncate (width chunk1))
    ;;          :chunk-height (truncate (height chunk1))
    ;;          :texture-atlas-ssbo texture-atlas-ssbo
    ;;          :atlas-size *texture-atlas-size*
    ;;          :skylight-colour skylight-colour
    ;;          :sky-colour sky-colour
    ;;          :lod (float lod)
    ;;          )
    ;;   )
    
    
    ;; (maphash (lambda (offset entry)
    ;;            (render (car entry)))
    ;;          *chunks-at-offsets-table*)
    tri-total)

  (defmethod render ((chunk chunk))
    (let ((buffer-stream (buffer-stream chunk)))
      (when (and buffer-stream
                 (< 0 (buffer-stream-length buffer-stream))))
      (incf tri-total (/ (first (gpu-array-dimensions (index-array chunk))) 3))
      (map-g #'chunk-pipeline buffer-stream
             :cam-pos camera-current-pos
             :cam-rot camera-current-rot
             :now *now*
             :proj *projection-matrix*
             :chunk-width (truncate (width chunk))
             :chunk-height (truncate (height chunk))
             :texture-atlas-ssbo texture-atlas-ssbo
             :atlas-size *texture-atlas-size*
             :skylight-colour skylight-colour
             :sky-colour sky-colour
             :lod (float lod)))))

(defun all-chunk-vert-arrays ()
  (let (result)
    (maphash (lambda (offset entry)
               (push (vert-array (car entry)) result))
             *chunks-at-offsets-table*)
    result))

(defun c-arrays-lengths (c-arrays)
  (mapcar #'first (mapcar #'c-array-dimensions c-arrays)))

(defun c-arrays-total-length (c-arrays)
  (apply #'+ (c-arrays-lengths c-arrays)))

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

;; (defun try-queue-chunk (chunk-offset block-positions-and-symbols width height)
;;   (if (queue-full?)
;;       (progn (sleep 0.0001)
;;              (try-queue-chunk chunk-offset block-positions-and-symbols width height))
;;       (queue-chunk (make-chunk-mesh-from-data block-positions-and-symbols chunk-offset width height)
;;                    (setq my-offset1 chunk-offset)
;;                    width
;;                    height)))

(defun make-chunk-mesh-from-data (block-positions-and-symbols chunk-offset chunk-width chunk-height)
  "Returns the mesh-data for a chunk made of the given block-symbols at given block-positions."
  (declare (optimize (speed 3) (safety 0))
           (type fixnum chunk-width chunk-height))
  (let ((chunk-x (first chunk-offset))
        (chunk-y (second chunk-offset))
        (chunk-z (third chunk-offset)))
    (declare (type fixnum chunk-x chunk-y chunk-z))
    ;;(setq my-offset4 (list chunk-x chunk-y chunk-z))
    ;;(setq my-offset5 (vec3 chunk-x chunk-y chunk-z))
    (combine-blocks-verts-and-indices
     (make-blocks-verts-and-indices-from-positions-and-symbols
      block-positions-and-symbols
      chunk-x chunk-y chunk-z
      chunk-width
      chunk-height
      (vec3 (float chunk-x) (float chunk-y) (float chunk-z)))))
  )

(defun queue-full? ()
  (< chunk-queue-max-size (length queued-chunks)))

(defun queue-chunk (mesh-data offset width height)
  (push (list mesh-data offset width height) queued-chunks))
