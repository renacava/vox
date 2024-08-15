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

(defparameter *cube-verts* (list (v! (xyz-to-index 0 1 0) (xy-to-index 0 0)) ;;0   FRONT
                                 (v! (xyz-to-index 0 0 0) (xy-to-index 0 1))      ;;1
                                 (v! (xyz-to-index 1 0 0)  (xy-to-index 1 1))            ;;2
                                 (v! (xyz-to-index 1 1 0)  (xy-to-index 1 0)) ;;3

                                 (v! (xyz-to-index 0 1 1) (xy-to-index 0 0)) ;;4   BACK
                                 (v! (xyz-to-index 1 1 1) (xy-to-index 0 1))       ;;5
                                 (v! (xyz-to-index 1 0 1) (xy-to-index 1 1)) ;;6
                                 (v! (xyz-to-index 0 0 1) (xy-to-index 1 0))     ;;7

                                 (v! (xyz-to-index 0 1 0) (xy-to-index 1 1)) ;;8   LEFT
                                 (v! (xyz-to-index 0 0 0) (xy-to-index 1 0)) ;;9
                                 (v! (xyz-to-index 0 0 1) (xy-to-index 0 0)) ;;10
                                 (v! (xyz-to-index 0 1 1) (xy-to-index 0 1)) ;;11

                                 (v! (xyz-to-index 1 1 0) (xy-to-index 0 1)) ;;12   RIGHT
                                 (v! (xyz-to-index 1 1 1) (xy-to-index 0 0)) ;;13
                                 (v! (xyz-to-index 1 0 0) (xy-to-index 1 1)) ;;14
                                 (v! (xyz-to-index 1 0 1) (xy-to-index 1 0)) ;;15

                                 (v! (xyz-to-index 0 1 1) (xy-to-index 0 0)) ;;16  TOP
                                 (v! (xyz-to-index 0 1 0) (xy-to-index 0 1)) ;;17
                                 (v! (xyz-to-index 1 1 0) (xy-to-index 1 1)) ;;18
                                 (v! (xyz-to-index 1 1 1) (xy-to-index 1 0)) ;;19

                                 (v! (xyz-to-index 0 0 1) (xy-to-index 0 1)) ;;20  BOTTOM
                                 (v! (xyz-to-index 1 0 0) (xy-to-index 1 0)) ;;21
                                 (v! (xyz-to-index 0 0 0) (xy-to-index 0 0)) ;;22
                                 (v! (xyz-to-index 1 0 1) (xy-to-index 1 1)) ;;23
                                 ))

;; (defparameter *cube-verts* (list (make-ivec2 (xyz-to-index 0 1 0) (xy-to-index 0 0)) ;;0   FRONT
;;                                  (make-ivec2 (xyz-to-index 0 0 0) (xy-to-index 0 1))      ;;1
;;                                  (make-ivec2 (xyz-to-index 1 0 0)  (xy-to-index 1 1))            ;;2
;;                                  (make-ivec2 (xyz-to-index 1 1 0)  (xy-to-index 1 0)) ;;3

;;                                  (make-ivec2 (xyz-to-index 0 1 1) (xy-to-index 0 0)) ;;4   BACK
;;                                  (make-ivec2 (xyz-to-index 1 1 1) (xy-to-index 0 1))       ;;5
;;                                  (make-ivec2 (xyz-to-index 1 0 1) (xy-to-index 1 1)) ;;6
;;                                  (make-ivec2 (xyz-to-index 0 0 1) (xy-to-index 1 0))     ;;7

;;                                  (make-ivec2 (xyz-to-index 0 1 0) (xy-to-index 1 1)) ;;8   LEFT
;;                                  (make-ivec2 (xyz-to-index 0 0 0) (xy-to-index 1 0)) ;;9
;;                                  (make-ivec2 (xyz-to-index 0 0 1) (xy-to-index 0 0)) ;;10
;;                                  (make-ivec2 (xyz-to-index 0 1 1) (xy-to-index 0 1)) ;;11

;;                                  (make-ivec2 (xyz-to-index 1 1 0) (xy-to-index 0 1)) ;;12   RIGHT
;;                                  (make-ivec2 (xyz-to-index 1 1 1) (xy-to-index 0 0)) ;;13
;;                                  (make-ivec2 (xyz-to-index 1 0 0) (xy-to-index 1 1)) ;;14
;;                                  (make-ivec2 (xyz-to-index 1 0 1) (xy-to-index 1 0)) ;;15

;;                                  (make-ivec2 (xyz-to-index 0 1 1) (xy-to-index 0 0)) ;;16  TOP
;;                                  (make-ivec2 (xyz-to-index 0 1 0) (xy-to-index 0 1)) ;;17
;;                                  (make-ivec2 (xyz-to-index 1 1 0) (xy-to-index 1 1)) ;;18
;;                                  (make-ivec2 (xyz-to-index 1 1 1) (xy-to-index 1 0)) ;;19

;;                                  (make-ivec2 (xyz-to-index 0 0 1) (xy-to-index 0 1)) ;;20  BOTTOM
;;                                  (make-ivec2 (xyz-to-index 1 0 0) (xy-to-index 1 0)) ;;21
;;                                  (make-ivec2 (xyz-to-index 0 0 0) (xy-to-index 0 0)) ;;22
;;                                  (make-ivec2 (xyz-to-index 1 0 1) (xy-to-index 1 1)) ;;23
;;                                  ))

;; (defparameter *cube-verts* (list (list (v! -0.5 0.5 -0.5) (v! 0 0)) ;;0   FRONT
;;                                  (list (v! -0.5 -0.5 -0.5) (v! 0 0.5))      ;;1
;;                                  (list (v! 0.5 -0.5 -0.5)  (v! 0.5 0.5))            ;;2
;;                                  (list (v! 0.5 0.5 -0.5)  (v! 0.5 0)) ;;3

;;                                  (list (v! -0.5 0.5 0.5) (v! 0 0)) ;;4   BACK
;;                                  (list (v! 0.5 0.5 0.5) (v! 0 0.5))       ;;5
;;                                  (list (v! 0.5 -0.5 0.5) (v! 0.5 0.5)) ;;6
;;                                  (list (v! -0.5 -0.5 0.5) (v! 0.5 0))     ;;7

;;                                  (list (v! -0.5 0.5 -0.5) (v! 0.5 0.5)) ;;8   LEFT
;;                                  (list (v! -0.5 -0.5 -0.5) (v! 0.5 0)) ;;9
;;                                  (list (v! -0.5 -0.5 0.5) (v! 0 0)) ;;10
;;                                  (list (v! -0.5 0.5 0.5) (v! 0 0.5)) ;;11

;;                                  (list (v! 0.5 0.5 -0.5) (v! 0 0.5)) ;;12   RIGHT
;;                                  (list (v! 0.5 0.5 0.5) (v! 0 0)) ;;13
;;                                  (list (v! 0.5 -0.5 -0.5) (v! 0.5 0.5)) ;;14
;;                                  (list (v! 0.5 -0.5 0.5) (v! 0.5 0)) ;;15

;;                                  (list (v! -0.5 0.5 0.5) (v! 0 0)) ;;16  TOP
;;                                  (list (v! -0.5 0.5 -0.5) (v! 0 0.5)) ;;17
;;                                  (list (v! 0.5 0.5 -0.5) (v! 0.5 0.5)) ;;18
;;                                  (list (v! 0.5 0.5 0.5) (v! 0.5 0)) ;;19

;;                                  (list (v! -0.5 -0.5 0.5) (v! 0 0.5)) ;;20  BOTTOM
;;                                  (list (v! 0.5 -0.5 -0.5) (v! 0.5 0)) ;;21
;;                                  (list (v! -0.5 -0.5 -0.5) (v! 0 0)) ;;22
;;                                  (list (v! 0.5 -0.5 0.5) (v! 0.5 0.5)) ;;23
;;                                  ))

(declaim (type fixnum *cube-n-verts*)
         (type (simple-array fixnum) *cube-indices*))
(defparameter *cube-n-verts* (length *cube-verts*))

(defparameter *cube-indices* (make-array 36 :element-type 'fixnum
                                            :initial-contents (vector 2 1 0 3 2 0
                                                                      6 5 4 7 6 4
                                                                      9 10 8 10 11 8
                                                                      15 14 12 13 15 12
                                                                      18 17 16 19 18 16
                                                                      22 21 20 21 23 20)
                                         ))

(defun offset-vert (vert offset-index)
  (declare (optimize (speed 3) (safety 0)))
  (vec2 (+ (aref vert 0) offset-index) (aref vert 1)))

(defun make-block-verts-and-indices (offset &optional (index-offset 0))
  (declare (optimize (speed 3) (safety 3))
           (type fixnum offset index-offset))
  (setf index-offset (* index-offset *cube-n-verts*))
  (list (mapcar (lambda (vert) (offset-vert vert offset))
                *cube-verts*)
        (loop for index fixnum across *cube-indices*
                      collect (+ index index-offset))
        ))

(defun make-blocks-verts-and-indices (offsets)
  (let ((index-offset -1))
    (loop for offset in offsets
          when offset
          collect (make-block-verts-and-indices offset (incf index-offset)))))

(defun combine-blocks-verts-and-indices (blocks-verts-and-indices)
  (let* ((vert-vecs (mapcar #'first blocks-verts-and-indices))
         (index-lists (mapcar #'second blocks-verts-and-indices))
         (verts (loop for vert-list in vert-vecs append vert-list)))
    (setq my-stuff verts)
    
    (list ;;(apply #'concatenate 'vector vert-vecs)
     
     (make-c-array ;; (apply #'concatenate 'vector verts)
      verts :element-type :vec2
                   )
     (make-c-array (apply #'concatenate 'list index-lists) :element-type :uint))))
