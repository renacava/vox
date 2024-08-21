(in-package #:vox)

(defparameter *chunk-width* 16)
(defparameter *chunk-height* 128)
(defparameter *chunk-size* (* *chunk-width* *chunk-width* *chunk-height*))

(defun set-chunk-width (&optional (width 16))
  (setf *chunk-width* width)
  (setf *chunk-size* (* *chunk-width* *chunk-width* *chunk-height*)))

(defun set-chunk-height (&optional (height 128))
  (setf *chunk-height* height)
  (setf *chunk-size* (* *chunk-width* *chunk-width* *chunk-height*)))

(defun 3d-to-1d (x y z &optional (cols *chunk-width*) (depth *chunk-height*))
  (+ x (* y cols) (* z cols depth)))

;; (defun 1d-to-3d (index &optional (cols *chunk-width*) (depth cols))
;;   (let* ((z (truncate (/ index (* cols depth))))
;;          (index (- index (* z cols depth)))
;;          (x (mod index cols))
;;          (y (truncate (/ index cols))))
;;     (list x y z)))

(defun 2d-to-1d (x y &optional (cols *chunk-width*))
  (+ x (* y cols)))

;; (defun 1d-to-2d (index &optional (cols *chunk-width*))
;;   (let* ((x (mod index cols))
;;          (y (truncate (/ index cols))))
;;     (list x y)))

(defparameter cube-back 
  (list
   (list
    (list (3d-to-1d 0.0 1.0 0.0) (2d-to-1d 0.0 1.0))
    (list (3d-to-1d 0.0 0.0 0.0) (2d-to-1d 0.0 0.0))
    (list (3d-to-1d 1.0 0.0 0.0)  (2d-to-1d 1.0 1.0))
    (list (3d-to-1d 1.0 1.0 0.0)  (2d-to-1d 1.0 0.0))
    )
   (list 2 1 0 3 2 0)))

(defparameter cube-front
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
           (list (3d-to-1d 0.0 0.0 1.0) (2d-to-1d 0.0 0.0)) ;;0
           (list (3d-to-1d 1.0 0.0 0.0) (2d-to-1d 1.0 1.0)) ;;1
           (list (3d-to-1d 0.0 0.0 0.0) (2d-to-1d 0.0 1.0)) ;;2
           (list (3d-to-1d 1.0 0.0 1.0) (2d-to-1d 1.0 0.0)) ;;3
           )
   (list ;;2 1 0 1 3 0
    ;;2 1 0 1 3 1
    2 1 0 1 3 0
         )))

(let ((cache (make-hash-table :test #'equal)))
  (defun build-cube-mesh-from-faces (faces)
    (or (gethash faces cache)
        (setf (gethash faces cache) (combine-cube-faces (get-cube-faces faces))))))

(defun augment-cube-mesh-with-block-symbol-and-offset (cube-mesh block-symbol offset &optional (index-offset 0))
  (let ((verts (first cube-mesh))
        (mesh-instance (get-mesh-bound-to-block-symbol block-symbol))
        (index-offset (* index-offset )))
    (list (loop for vert in verts
                collect (append vert (list (2d-to-1d (getf mesh-instance :atlas-column)
                                                     (getf mesh-instance :atlas-row))
                                           (3d-to-1d (float (first offset))
                                                     
                                                     (float (second offset))
                                                     (float (third offset)))
                                           
                                           1
                                           ;;(if (> (random 3) 1) 1 0)
                                           )))
          (second cube-mesh))))

(defun get-cube-faces (faces)
  (loop for face in faces
        collect (case face
                  (front cube-front)
                  (back cube-back)
                  (left cube-left)
                  (right cube-right)
                  (top cube-top)
                  (bottom cube-bottom))))

(defun combine-cube-faces (cube-faces)
  (let* ((index-offset 0)
         (vert-lists (mapcar #'first cube-faces))
         (index-lists (mapcar #'second cube-faces))
         (verts (loop for vert-list in vert-lists
                      append vert-list))
         (indices (loop for index-list in index-lists
                        for vert-list in vert-lists
                        append (loop for index in index-list
                                     collect (+ index-offset index))
                        do (incf index-offset (length vert-list)))))
    
    (list verts
          
          indices
          ;; (make-array (length indices)
          ;;             :element-type 'fixnum
          ;;             :initial-contents indices)
          )))

(let ((cube-mesh (build-cube-mesh-from-faces `(front back left right top bottom))))
  (defparameter *cube-verts* (first cube-mesh))
  (defparameter *cube-n-verts* (length *cube-verts*))
  (defparameter *cube-indices* (second cube-mesh)))


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
