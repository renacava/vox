(in-package #:vox)

(defconstant *cube-verts* (list (v! -0.5 0.5 -0.5) ;;0   FRONT
                                (v! -0.5 -0.5 -0.5) ;;1
                                (v! 0.5 -0.5 -0.5) ;;2
                                (v! 0.5 0.5 -0.5) ;;3

                                (v! -0.5 0.5 0.5) ;;4   BACK
                                (v! 0.5 0.5 0.5) ;;5
                                (v! 0.5 -0.5 0.5) ;;6
                                (v! -0.5 -0.5 0.5) ;;7

                                (v! -0.5 0.5 -0.5) ;;8   LEFT
                                (v! -0.5 -0.5 -0.5) ;;9
                                (v! -0.5 -0.5 0.5) ;;10
                                (v! -0.5 0.5 0.5) ;;11

                                (v! 0.5 0.5 -0.5) ;;12   RIGHT
                                (v! 0.5 0.5 0.5) ;;13
                                (v! 0.5 -0.5 -0.5) ;;14
                                (v! 0.5 -0.5 0.5) ;;15

                                (v! -0.5 0.5 0.5) ;;16  TOP
                                (v! -0.5 0.5 -0.5) ;;17
                                (v! 0.5 0.5 -0.5) ;;18
                                (v! 0.5 0.5 0.5) ;;19

                                (v! -0.5 -0.5 0.5) ;;20  BOTTOM
                                (v! 0.5 -0.5 -0.5) ;;21
                                (v! -0.5 -0.5 -0.5) ;;22
                                (v! 0.5 -0.5 0.5) ;;23
                                ))

(defconstant *cube-n-verts* (length *cube-verts*))

(defconstant *cube-indices* (list 2 1 0 3 2 0
                                  6 5 4 7 6 4
                                  9 10 8 10 11 8
                                  15 14 12 13 15 12
                                  18 17 16 19 18 16
                                  22 21 20 21 23 20))

(defun offset-vec3 (vec x-offset y-offset z-offset)
  (vec3 (+ (aref vec 0) x-offset)
        (+ (aref vec 1) y-offset)
        (+ (aref vec 2) z-offset)))

(defun make-block-verts-and-indices (x-offset y-offset z-offset &optional (index-offset 0))
  (setf index-offset (* index-offset *cube-n-verts*))
  (list (mapcar (lambda (vec) (offset-vec3 vec x-offset y-offset z-offset)) *cube-verts*)
          (loop for index in *cube-indices*
                collect (+ index index-offset))))

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
