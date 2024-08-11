(in-package #:vox)

(defparameter *vert-gpu-array* nil)
(defparameter *vert-gpu-index-array* nil)
(defparameter *vert-array-buffer-stream* nil)
(defparameter *projection-matrix* nil)

(defun try-free (object)
  (when object (free object)))

(defun try-free-objects (&rest objects)
  (mapcar #'try-free objects))

(defun-g vert-stage ((vert :vec3)
                     &uniform
                     (now :float)
                     (proj :mat4)
                     (rot :vec3))
  (let* ((pos (* (rtg-math.matrix4:rotation-from-euler rot) (vec4 vert 1)))
         (pos (+ pos (vec4 (* 2 (sin now)) (* 3 (cos now)) -5 0))))
    (values (* proj pos)
            vert)))

(defun-g frag-stage ((col :vec3))
  (let ((col (+ col (vec3 0.5 0.5 0.5))))
    (vec4 col 1)))

(defpipeline-g basic-pipeline ()
  (vert-stage :vec3)
  (frag-stage :vec3))

(defun now ()
  (float (/ (get-internal-real-time) 1000)))

(defun init ()

  
  (try-free-objects *vert-gpu-array* *vert-gpu-index-array* *vert-array-buffer-stream*)
  (let ((my-chunk (make-chunk-buffer-stream)))
    (setf *vert-gpu-index-array* (second my-chunk)
          *vert-gpu-array* (first my-chunk)
          *vert-array-buffer-stream* (third my-chunk)))
  
  ;; (setf *vert-gpu-index-array* (make-gpu-array (list 2 1 0 3 2 0
  ;;                                                    6 5 4 7 6 4
  ;;                                                    9 10 8 10 11 8
  ;;                                                    15 14 12 13 15 12
  ;;                                                    18 17 16 19 18 16
  ;;                                                    22 21 20 21 23 20)
  ;;                                              :element-type :uint))
  ;; (setf *vert-gpu-array* (make-gpu-array
  ;;                         (list (v! -0.5 0.5 -0.5) ;;0   FRONT
  ;;                               (v! -0.5 -0.5 -0.5) ;;1
  ;;                               (v! 0.5 -0.5 -0.5) ;;2
  ;;                               (v! 0.5 0.5 -0.5) ;;3

  ;;                               (v! -0.5 0.5 0.5) ;;4   BACK
  ;;                               (v! 0.5 0.5 0.5) ;;5
  ;;                               (v! 0.5 -0.5 0.5) ;;6
  ;;                               (v! -0.5 -0.5 0.5) ;;7

  ;;                               (v! -0.5 0.5 -0.5) ;;8   LEFT
  ;;                               (v! -0.5 -0.5 -0.5) ;;9
  ;;                               (v! -0.5 -0.5 0.5) ;;10
  ;;                               (v! -0.5 0.5 0.5) ;;11

  ;;                               (v! 0.5 0.5 -0.5) ;;12   RIGHT
  ;;                               (v! 0.5 0.5 0.5) ;;13
  ;;                               (v! 0.5 -0.5 -0.5) ;;14
  ;;                               (v! 0.5 -0.5 0.5) ;;15

  ;;                               (v! -0.5 0.5 0.5) ;;16  TOP
  ;;                               (v! -0.5 0.5 -0.5) ;;17
  ;;                               (v! 0.5 0.5 -0.5) ;;18
  ;;                               (v! 0.5 0.5 0.5) ;;19

  ;;                               (v! -0.5 -0.5 0.5) ;;20  BOTTOM
  ;;                               (v! 0.5 -0.5 -0.5) ;;21
  ;;                               (v! -0.5 -0.5 -0.5) ;;22
  ;;                               (v! 0.5 -0.5 0.5) ;;23
  ;;                               )))
  ;; (setf *vert-array-buffer-stream* (make-buffer-stream *vert-gpu-array* :index-array *vert-gpu-index-array*))
  (setf *projection-matrix* (rtg-math.projection:perspective (x (resolution (current-viewport)))
                                                              (y (resolution (current-viewport)))
                                                              0.1
                                                              30f0
                                                              60f0)))

(defun step-rendering ()
  (clear)
  (map-g #'basic-pipeline *vert-array-buffer-stream*
         :now (now)
         :proj *projection-matrix*
         :rot (v! (* 90 0.03 (now)) (* 90 0.02 (now)) (* 90 0.01 (now))))
  (step-host)
  (swap))


(defparameter main-loop-func (lambda ()
                               (step-rendering)
                               (step-host)
                               (sleep 0.025)
                               ))

(defun main ()
  (cepl:repl)
  (init)
  (loop (funcall main-loop-func)))

(defun make-chunk-buffer-stream (&key (width 2) (height 2) (depth 2))
  "Returns a buffer-stream object for the mesh of a chunk of the given dimensions, made of cubes."
  (let* ((block-indices (make-chunk-block-indices :width width :height height :depth depth))
         (blocks-verts-and-indices (make-blocks-verts-and-indices block-indices))
         (mesh-data (combine-blocks-verts-and-indices blocks-verts-and-indices))
         (verts-gpu-array (make-gpu-array (coerce (first mesh-data) 'list)))
         (indices-gpu-array (make-gpu-array (second mesh-data) :element-type :uint)))
    (list verts-gpu-array
          indices-gpu-array
          (make-buffer-stream verts-gpu-array :index-array indices-gpu-array))
    
    
    ))

(defun make-chunk-block-indices (&key (width 2) (height 2) (depth 2))
  (loop for x below width
        append (loop for y below height
                     append (loop for z below depth
                                  collect (list x y z)))))

