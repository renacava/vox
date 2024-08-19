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

(defparameter *cube-verts* (list (list (vec3 0.0 1.0 0.0) (vec2 0.0 1.0)) ;;0.0   FRONT
                                 (list (vec3 0.0 0.0 0.0) (vec2 0.0 0.0))      ;;1.0
                                 (list (vec3 1.0 0.0 0.0)  (vec2 1.0 1.0))            ;;2
                                 (list (vec3 1.0 1.0 0.0)  (vec2 1.0 0.0)) ;;3

                                 (list (vec3 0.0 1.0 1.0) (vec2 0.0 0.0)) ;;4   BACK
                                 (list (vec3 1.0 1.0 1.0) (vec2 1.0 0.0))       ;;5
                                 (list (vec3 1.0 0.0 1.0) (vec2 1.0 1.0)) ;;6
                                 (list (vec3 0.0 0.0 1.0) (vec2 0.0 1.0))     ;;7

                                 (list (vec3 0.0 1.0 0.0) (vec2 0.0 0.0)) ;;8   LEFT
                                 (list (vec3 0.0 0.0 0.0) (vec2 0.0 1.0)) ;;9
                                 (list (vec3 0.0 0.0 1.0) (vec2 1.0 1.0)) ;;1.00.0
                                 (list (vec3 0.0 1.0 1.0) (vec2 1.0 0.0)) ;;1.01.0

                                 (list (vec3 1.0 1.0 0.0) (vec2 1.0 0.0)) ;;1.02   RIGHT
                                 (list (vec3 1.0 1.0 1.0) (vec2 0.0 0.0)) ;;1.03
                                 (list (vec3 1.0 0.0 0.0) (vec2 1.0 1.0)) ;;1.04
                                 (list (vec3 1.0 0.0 1.0) (vec2 0.0 1.0)) ;;1.05

                                 (list (vec3 0.0 1.0 1.0) (vec2 0.0 1.0)) ;;1.06  TOP
                                 (list (vec3 0.0 1.0 0.0) (vec2 0.0 0.0)) ;;1.07
                                 (list (vec3 1.0 1.0 0.0) (vec2 1.0 0.0)) ;;1.08
                                 (list (vec3 1.0 1.0 1.0) (vec2 1.0 1.0)) ;;1.09

                                 (list (vec3 0.0 0.0 1.0) (vec2 0.0 0.0)) ;;20.0  BOTTOM
                                 (list (vec3 1.0 0.0 0.0) (vec2 1.0 1.0)) ;;21.0
                                 (list (vec3 0.0 0.0 0.0) (vec2 0.0 1.0)) ;;22
                                 (list (vec3 1.0 0.0 1.0) (vec2 1.0 0.0)) ;;23
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

(defun make-block-verts-and-indices (offset &optional (index-offset 0) (block-symbol nil))
  (declare (optimize (speed 3) (safety 3))
           (type fixnum index-offset))
  (let ((mesh (get-mesh-bound-to-block-symbol block-symbol)))
    (setf index-offset (* index-offset (getf mesh :n-verts)))
    (list (mapcar (lambda (vert)
                    (append vert (list (getf mesh :atlas-column)
                                       (getf mesh :atlas-row)
                                       (vec3 (float (first offset))
                                             (float (second offset))
                                             (float (third offset))))))
                  (getf mesh :verts))
          (loop for index fixnum across (getf mesh :indices)
                collect (+ index index-offset)))))

(defun make-blocks-verts-and-indices-from-positions-and-symbols (positions-and-symbols)
  (let ((index-offset -1))
    (loop for pos-and-symb in positions-and-symbols
          collect (make-block-verts-and-indices (subseq pos-and-symb 0 3)
                                                (incf index-offset)
                                                (last1 pos-and-symb)))))

(defparameter my-index-arrays nil)
(defparameter my-vert-arrays nil)

(defun combine-blocks-verts-and-indices (blocks-verts-and-indices)
  (let* ((vert-vecs (mapcar #'first blocks-verts-and-indices))
         (index-lists (mapcar #'second blocks-verts-and-indices))
         (verts (loop for vert-list in vert-vecs append vert-list))
         (indices (apply #'concatenate 'list index-lists))
         (vert-c-array  (make-c-array verts :element-type 'block-vert))
         (index-c-array (make-c-array indices :element-type :uint)))
    (push vert-c-array my-vert-arrays)
    (push index-c-array my-index-arrays)
    (list vert-c-array index-c-array)))
