(in-package #:vox)

(defun xy-to-index (x y)
  (+ x (* y 64)))

(defun index-to-xy (index)
  (vector (truncate (mod index 64))
          (truncate (mod (/ index 64) 64))))

(defun xyz-to-index (x y z)
  (+ x (* y 64) (* z 64 64)))

(defun index-to-xyz (index)
  (vector (truncate (mod index 64))
          (truncate (mod (/ index 64) 64))
          (truncate (mod (/ (/ index 64) 64) 64))))

(defun vec3-to-index (vec3)
  (+ (aref vec3 0)
     (* (aref vec3 1) 64)
     (* (aref vec3 2) 64 64)))

(defun make-ivec2 (x y)
  (make-array 2 :element-type `(signed-byte 32) :initial-contents (vector x y)))

(defun make-ivec3 (x y z)
  (make-array 3 :element-type `(signed-byte 32) :initial-contents (vector x y z)))

(defparameter *cube-verts* (list (list (vec3 0.0 1.0 0.0) (vec2 0.0 0.0)) ;;0.0   FRONT
                                 (list (vec3 0.0 0.0 0.0) (vec2 0.0 1.0))      ;;1.0
                                 (list (vec3 1.0 0.0 0.0)  (vec2 1.0 1.0))            ;;2
                                 (list (vec3 1.0 1.0 0.0)  (vec2 1.0 0.0)) ;;3

                                 (list (vec3 0.0 1.0 1.0) (vec2 0.0 0.0)) ;;4   BACK
                                 (list (vec3 1.0 1.0 1.0) (vec2 0.0 1.0))       ;;5
                                 (list (vec3 1.0 0.0 1.0) (vec2 1.0 1.0)) ;;6
                                 (list (vec3 0.0 0.0 1.0) (vec2 1.0 0.0))     ;;7

                                 (list (vec3 0.0 1.0 0.0) (vec2 1.0 1.0)) ;;8   LEFT
                                 (list (vec3 0.0 0.0 0.0) (vec2 1.0 0.0)) ;;9
                                 (list (vec3 0.0 0.0 1.0) (vec2 0.0 0.0)) ;;1.00.0
                                 (list (vec3 0.0 1.0 1.0) (vec2 0.0 1.0)) ;;1.01.0

                                 (list (vec3 1.0 1.0 0.0) (vec2 0.0 1.0)) ;;1.02   RIGHT
                                 (list (vec3 1.0 1.0 1.0) (vec2 0.0 0.0)) ;;1.03
                                 (list (vec3 1.0 0.0 0.0) (vec2 1.0 1.0)) ;;1.04
                                 (list (vec3 1.0 0.0 1.0) (vec2 1.0 0.0)) ;;1.05

                                 (list (vec3 0.0 1.0 1.0) (vec2 0.0 0.0)) ;;1.06  TOP
                                 (list (vec3 0.0 1.0 0.0) (vec2 0.0 1.0)) ;;1.07
                                 (list (vec3 1.0 1.0 0.0) (vec2 1.0 1.0)) ;;1.08
                                 (list (vec3 1.0 1.0 1.0) (vec2 1.0 0.0)) ;;1.09

                                 (list (vec3 0.0 0.0 1.0) (vec2 0.0 1.0)) ;;20.0  BOTTOM
                                 (list (vec3 1.0 0.0 0.0) (vec2 1.0 0.0)) ;;21.0
                                 (list (vec3 0.0 0.0 0.0) (vec2 0.0 0.0)) ;;22
                                 (list (vec3 1.0 0.0 1.0) (vec2 1.0 1.0)) ;;23
                                 ))

(declaim (type fixnum *cube-n-verts*)
         (type (simple-array fixnum) *cube-indices*))
(defparameter *cube-n-verts* (length *cube-verts*))

(defparameter *cube-indices* (make-array 36 :element-type 'fixnum
                                            :initial-contents (vector 2 1 0 3 2 0
                                                                      6 5 4 7 6 4
                                                                      9 10 8 10 11 8
                                                                      15 14 12 13 15 12
                                                                      18 17 16 19 18 16
                                                                      22 21 20 21 23 20)))

(defun offset-vert (vert offset-vec3 &optional (id 0.0))
  (declare (optimize (speed 3) (safety 1)))
  (let ((vert-vec3 (first vert))
        (uv (second vert)))
    (list (vec3 (+ (aref vert-vec3 0) (aref offset-vec3 0))
                (+ (aref vert-vec3 1) (aref offset-vec3 1))
                (+ (aref vert-vec3 2) (aref offset-vec3 2)))
          uv
          id)))

(defun make-block-verts-and-indices (offset &optional (index-offset 0))
  (declare (optimize (speed 3) (safety 3))
           (type fixnum index-offset))
  (setf index-offset (* index-offset *cube-n-verts*))
  (list (mapcar (lambda (vert)
                  (offset-vert vert offset))
                *cube-verts*)
        (loop for index fixnum across *cube-indices*
                      collect (+ index index-offset))))

(defun make-blocks-verts-and-indices (offsets)
  (let ((index-offset -1))
    (loop for offset in offsets
          when offset
          collect (make-block-verts-and-indices offset (incf index-offset)))))

(defun combine-blocks-verts-and-indices (blocks-verts-and-indices)
  (let* ((vert-vecs (mapcar #'first blocks-verts-and-indices))
         (index-lists (mapcar #'second blocks-verts-and-indices))
         (verts (loop for vert-list in vert-vecs append vert-list))
         (indices (apply #'concatenate 'list index-lists)))
    (list (make-c-array verts :element-type 'block-vert)
          (make-c-array indices :element-type :uint))))
