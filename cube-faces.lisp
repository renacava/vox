(in-package #:vox)

(defparameter *chunk-width* 16)

(defun set-chunk-width (&optional (width 16))
  (setf *chunk-width* width))

(defun coords-3d-to-1d (x y z &optional (cols *chunk-width*) (depth cols))
  "Transforms a 3d coordinate into a 1d one"
  (+ x (* y cols) (* z cols depth)))

(defun coords-1d-to-3d (n &optional (cols *chunk-width*) (depth cols))
  "Transforms a 1d coordinate into a 3d one"
  (list (mod n cols)
        (mod (truncate (/ n cols)) (* cols depth))
        (truncate (/ n (* cols depth)))))

(defun 3d-to-1d (x y z &optional (cols *chunk-width*) (depth cols))
  (+ x (* y cols) (* z cols depth)))

(defun 1d-to-3d (index &optional (cols *chunk-width*) (depth cols))
  (let* ((z (truncate (/ index (* cols depth))))
         (index (- index (* z cols depth)))
         (x (mod index cols))
         (y (truncate (/ index cols))))
    (list x y z)))

(defun 2d-to-1d (x y &optional (cols *chunk-width*))
  (+ x (* y cols)))

(defun 1d-to-2d (index &optional (cols *chunk-width*))
  (let* ((x (mod index cols))
         (y (truncate (/ index cols))))
    (list x y)))

(defparameter cube-front 
  (list
   (list
    (list (3d-to-1d 0.0 1.0 0.0) (2d-to-1d 0.0 1.0))
    (list (3d-to-1d 0.0 0.0 0.0) (2d-to-1d 0.0 0.0))
    (list (3d-to-1d 1.0 0.0 0.0)  (2d-to-1d 1.0 1.0))
    (list (3d-to-1d 1.0 1.0 0.0)  (2d-to-1d 1.0 0.0))
    )
   (list 2 1 0 3 2 0)))

(defparameter cube-back 
  (list
    (list
           (list (3d-to-1d 0.0 1.0 1.0) (2d-to-1d 0.0 0.0))
           (list (3d-to-1d 1.0 1.0 1.0) (2d-to-1d 1.0 0.0))
           (list (3d-to-1d 1.0 0.0 1.0) (2d-to-1d 1.0 1.0))
           (list (3d-to-1d 0.0 0.0 1.0) (2d-to-1d 0.0 1.0))
           )
    (list 2 1 0 3 2 0)))

(defparameter cube-left 
  (list
    (list
           (list (3d-to-1d 0.0 1.0 0.0) (2d-to-1d 0.0 0.0))
           (list (3d-to-1d 0.0 0.0 0.0) (2d-to-1d 0.0 1.0))
           (list (3d-to-1d 0.0 0.0 1.0) (2d-to-1d 1.0 1.0))
           (list (3d-to-1d 0.0 1.0 1.0) (2d-to-1d 1.0 0.0))
           )
    (list 1 2 0 2 3 0)))

(defparameter cube-right 
  (list
    (list
           (list (3d-to-1d 1.0 1.0 0.0) (2d-to-1d 1.0 0.0))
           (list (3d-to-1d 1.0 1.0 1.0) (2d-to-1d 0.0 0.0))
           (list (3d-to-1d 1.0 0.0 0.0) (2d-to-1d 1.0 1.0))
           (list (3d-to-1d 1.0 0.0 1.0) (2d-to-1d 0.0 1.0))
           )
    (list 3 2 0 1 3 0)))

(defparameter cube-top 
  (list
    (list
           (list (3d-to-1d 0.0 1.0 1.0) (2d-to-1d 0.0 1.0))
           (list (3d-to-1d 0.0 1.0 0.0) (2d-to-1d 0.0 0.0))
           (list (3d-to-1d 1.0 1.0 0.0) (2d-to-1d 1.0 0.0))
           (list (3d-to-1d 1.0 1.0 1.0) (2d-to-1d 1.0 1.0))
           )
    (list 2 1 0 3 2 0)))

(defparameter cube-bottom 
  (list
    (list
           (list (3d-to-1d 0.0 0.0 1.0) (2d-to-1d 0.0 0.0))
           (list (3d-to-1d 1.0 0.0 0.0) (2d-to-1d 1.0 1.0))
           (list (3d-to-1d 0.0 0.0 0.0) (2d-to-1d 0.0 1.0))
           (list (3d-to-1d 1.0 0.0 1.0) (2d-to-1d 1.0 0.0))
           )
    (list 2 1 0 1 3 1)))

(defun build-cube-mesh-from-faces (faces)
  (combine-cube-faces (get-cube-faces faces)))

(defun get-cube-faces (faces)
  (loop for face in faces
        collect (case face
                  (front cube-front)
                  (back cube-back)
                  (left cube-left)
                  (right cube-right)
                  (top cube-top)
                  (bottom cube-bottom)))
  )

(defun combine-cube-faces (cube-faces)
  (let ((index-offset 0)
        (vert-lists (mapcar #'first cube-faces))
        (index-lists (mapcar #'second cube-faces)))
    
    (list (loop for vert-list in vert-lists
                append vert-list)
          (loop for index-list in index-lists
                for vert-list in vert-lists
                append (loop for index in index-list
                             collect (+ index-offset index))
                do (incf index-offset (length vert-list))))))

(defparameter *cube-verts* (build-cube-mesh-from-faces `(front back left right top bottom)))

;; (declaim (type fixnum *cube-n-verts*)
;;          (type (simple-array fixnum) *cube-indices*))
;; (defparameter *cube-n-verts* (length *cube-verts*))

;; (defparameter *cube-indices* (make-array 36 :element-type 'fixnum
;;                                             :initial-contents (vector 2 1 0 3 2 0
;;                                                                       6 5 4 7 6 4
;;                                                                       9 10 8 10 11 8
;;                                                                       15 14 12 13 15 12
;;                                                                       18 17 16 19 18 16
;;                                                                       22 21 20 21 23 20)))
