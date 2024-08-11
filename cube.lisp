(in-package #:vox)

(defparameter *cube-verts* (list (list (v! -0.5 0.5 -0.5) (v! 0 0)) ;;0   FRONT
                                 (list (v! -0.5 -0.5 -0.5) (v! 0 0.5))      ;;1
                                 (list (v! 0.5 -0.5 -0.5)  (v! 0.5 0.5))            ;;2
                                 (list (v! 0.5 0.5 -0.5)  (v! 0.5 0)) ;;3

                                 (list (v! -0.5 0.5 0.5) (v! 0 0)) ;;4   BACK
                                 (list (v! 0.5 0.5 0.5) (v! 0 0.5))       ;;5
                                 (list (v! 0.5 -0.5 0.5) (v! 0.5 0.5)) ;;6
                                 (list (v! -0.5 -0.5 0.5) (v! 0.5 0))     ;;7

                                 (list (v! -0.5 0.5 -0.5) (v! 0.5 0.5)) ;;8   LEFT
                                 (list (v! -0.5 -0.5 -0.5) (v! 0.5 0)) ;;9
                                 (list (v! -0.5 -0.5 0.5) (v! 0 0)) ;;10
                                 (list (v! -0.5 0.5 0.5) (v! 0 0.5)) ;;11

                                 (list (v! 0.5 0.5 -0.5) (v! 0 0.5)) ;;12   RIGHT
                                 (list (v! 0.5 0.5 0.5) (v! 0 0)) ;;13
                                 (list (v! 0.5 -0.5 -0.5) (v! 0.5 0.5)) ;;14
                                 (list (v! 0.5 -0.5 0.5) (v! 0.5 0)) ;;15

                                 (list (v! -0.5 0.5 0.5) (v! 0 0)) ;;16  TOP
                                 (list (v! -0.5 0.5 -0.5) (v! 0 0.5)) ;;17
                                 (list (v! 0.5 0.5 -0.5) (v! 0.5 0.5)) ;;18
                                 (list (v! 0.5 0.5 0.5) (v! 0.5 0)) ;;19

                                 (list (v! -0.5 -0.5 0.5) (v! 0 0.5)) ;;20  BOTTOM
                                 (list (v! 0.5 -0.5 -0.5) (v! 0.5 0)) ;;21
                                 (list (v! -0.5 -0.5 -0.5) (v! 0 0)) ;;22
                                 (list (v! 0.5 -0.5 0.5) (v! 0.5 0.5)) ;;23
                                 ))

(defparameter *cube-n-verts* (length *cube-verts*))

(defparameter *cube-indices* (list 2 1 0 3 2 0
                                  6 5 4 7 6 4
                                  9 10 8 10 11 8
                                  15 14 12 13 15 12
                                  18 17 16 19 18 16
                                  22 21 20 21 23 20))

(defun offset-vec3 (vec x-offset y-offset z-offset)
  (let ((vert (first vec))
        (uv (second vec)))
    (list (vec3 (+ (aref vert 0) x-offset)
                (+ (aref vert 1) y-offset)
                (+ (aref vert 2) z-offset))
          uv))
  )

(defun make-block-verts-and-indices (x-offset y-offset z-offset &optional (index-offset 0))
  (setf index-offset (* index-offset *cube-n-verts*))
  (list (mapcar (lambda (vec) (offset-vec3 vec x-offset y-offset z-offset)) *cube-verts*)
        (loop for index in *cube-indices*
                      collect (+ index index-offset))
        ))

(defun make-blocks-verts-and-indices (offsets)
  (let ((index-offset -1))
    (loop for offset in offsets
          when offset
          collect (make-block-verts-and-indices (first offset) (second offset) (third offset) (incf index-offset))))
  )

(defun combine-blocks-verts-and-indices (blocks-verts-and-indices)
  (let ((vert-vecs (mapcar #'first blocks-verts-and-indices))
        (index-lists (mapcar #'second blocks-verts-and-indices)))
    (list ;;(apply #'concatenate 'vector vert-vecs)
     (loop for vert-list in vert-vecs
           append vert-list)
          (apply #'concatenate 'list index-lists))))
