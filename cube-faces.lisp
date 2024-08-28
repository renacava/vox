(in-package #:vox)

(defparameter *chunk-width* 16)
(defparameter *chunk-height* 128)
(defparameter *chunk-size* (* *chunk-width* *chunk-width* *chunk-height*))

(defparameter *texture-atlas-tex* nil)
(defparameter *texture-atlas-sampler* nil)
(defparameter *texture-atlas-size* 1f0)
(defparameter *texture-cell-size* 16)

(defun face-direction-to-float (face-direction)
  (float
   (case face-direction
     (up 0f0)
     (top 0f0)

     (left 1f0)

     (right 2f0)

     (forward 3f0)
     (front 3f0)
     (ahead 3f0)

     (back 4f0)
     (backwards 4f0)
     
     (down 5f0)
     (bottom 5f0)
     
     (t 1.0f0))))

(defun resolve-textures ()
  (load-texture-atlas)
  (setup-mesh-table))

(defun set-chunk-width (&optional (width 16))
  (setf *chunk-width* width)
  (setf *chunk-size* (* *chunk-width* *chunk-width* *chunk-height*)))

(defun set-chunk-height (&optional (height 128))
  (setf *chunk-height* height)
  (setf *chunk-size* (* *chunk-width* *chunk-width* *chunk-height*)))

(defun 3d-to-1d (x y z &optional (cols *chunk-width*) (depth *chunk-height*))
  (+ x (* y cols) (* z cols depth)))

(defun 2d-to-1d (x y &optional (cols *texture-atlas-size*))
  (+ x (* y cols)))

(defun setup-cube-faces ()
  (defparameter cube-back
    (let ((face-float (face-direction-to-float 'front)))
      (list
       (list
        (list (3d-to-1d 0.0 1.0 0.0) (2d-to-1d 0.0 1.0) face-float)
        (list (3d-to-1d 0.0 0.0 0.0) (2d-to-1d 0.0 0.0) face-float)
        (list (3d-to-1d 1.0 0.0 0.0)  (2d-to-1d 1.0 1.0) face-float)
        (list (3d-to-1d 1.0 1.0 0.0)  (2d-to-1d 1.0 0.0) face-float))
       (list 2 1 0 3 2 0))))

  (defparameter cube-front
    (let ((face-float (face-direction-to-float 'back)))
      (list
       (list
        (list (3d-to-1d 0.0 1.0 1.0) (2d-to-1d 0.0 0.0) face-float)
        (list (3d-to-1d 1.0 1.0 1.0) (2d-to-1d 1.0 0.0) face-float)
        (list (3d-to-1d 1.0 0.0 1.0) (2d-to-1d 1.0 1.0) face-float)
        (list (3d-to-1d 0.0 0.0 1.0) (2d-to-1d 0.0 1.0) face-float))
       (list 2 1 0 3 2 0))))

  (defparameter cube-left 
    (let ((face-float (face-direction-to-float 'left)))
      (list
       (list
        (list (3d-to-1d 0.0 1.0 0.0) (2d-to-1d 0.0 0.0) face-float)
        (list (3d-to-1d 0.0 0.0 0.0) (2d-to-1d 0.0 1.0) face-float)
        (list (3d-to-1d 0.0 0.0 1.0) (2d-to-1d 1.0 1.0) face-float)
        (list (3d-to-1d 0.0 1.0 1.0) (2d-to-1d 1.0 0.0) face-float))
       (list 1 2 0 2 3 0))))

  (defparameter cube-right 
    (let ((face-float (face-direction-to-float 'right)))
      (list
       (list
        (list (3d-to-1d 1.0 1.0 0.0) (2d-to-1d 1.0 0.0) face-float)
        (list (3d-to-1d 1.0 1.0 1.0) (2d-to-1d 0.0 0.0) face-float)
        (list (3d-to-1d 1.0 0.0 0.0) (2d-to-1d 1.0 1.0) face-float)
        (list (3d-to-1d 1.0 0.0 1.0) (2d-to-1d 0.0 1.0) face-float))
       (list 3 2 0 1 3 0))))

  (defparameter cube-top 
    (let ((face-float (face-direction-to-float 'top)))
      (list
       (list
        (list (3d-to-1d 0.0 1.0 1.0) (2d-to-1d 0.0 1.0) face-float)
        (list (3d-to-1d 0.0 1.0 0.0) (2d-to-1d 0.0 0.0) face-float)
        (list (3d-to-1d 1.0 1.0 0.0) (2d-to-1d 1.0 0.0) face-float)
        (list (3d-to-1d 1.0 1.0 1.0) (2d-to-1d 1.0 1.0) face-float))
       (list 2 1 0 3 2 0))))

  (defparameter cube-bottom 
    (let ((face-float (face-direction-to-float 'bottom)))
      (list
       (list
        (list (3d-to-1d 0.0 0.0 1.0) (2d-to-1d 0.0 0.0) face-float)
        (list (3d-to-1d 1.0 0.0 0.0) (2d-to-1d 1.0 1.0) face-float)
        (list (3d-to-1d 0.0 0.0 0.0) (2d-to-1d 0.0 1.0) face-float)
        (list (3d-to-1d 1.0 0.0 1.0) (2d-to-1d 1.0 0.0) face-float))
       (list 2 1 0 1 3 0))))

  (let ((cache (make-hash-table :test #'equal)))
    (defun build-cube-mesh-from-faces (faces)
      (or (gethash faces cache)
          (setf (gethash faces cache) (combine-cube-faces (get-cube-faces (remove-duplicates faces))))))))

(defun augment-cube-mesh-with-block-symbol-and-offset (cube-mesh block-symbol offset &optional (highest-block-in-chunk? nil))
  (let ((verts (first cube-mesh))
        (mesh-instance (get-mesh-bound-to-block-symbol block-symbol)))
    (list (loop for vert in verts
                collect (let* ((pos-and-uv (subseq vert 0 2))
                               (face-float (third vert))
                               (face-float (if highest-block-in-chunk? (+ 6f0 face-float) face-float)))
                          (append pos-and-uv
                                  (list
                                   face-float
                                   (2d-to-1d (getf mesh-instance :atlas-column)
                                             (getf mesh-instance :atlas-row))
                                   (3d-to-1d (float (first offset))
                                             
                                             (float (second offset))
                                             (float (third offset)))))))
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
    
    (list verts indices)))

(defun setup-default-cube-mesh ()
  (setup-cube-faces)
  (let ((cube-mesh (build-cube-mesh-from-faces `(front back left right top bottom))))
    (defparameter *cube-verts* (first cube-mesh))
    (defparameter *cube-n-verts* (length *cube-verts*))
    (defparameter *cube-indices* (second cube-mesh))))

(setup-default-cube-mesh)

