(in-package #:vox)

(defparameter *chunk-width* 16)
(defparameter *chunk-height* 128)
(defparameter *chunk-size* (* *chunk-width* *chunk-width* *chunk-height*))

(defparameter *texture-atlas-tex* nil)
(defparameter *texture-atlas-sampler* nil)
(defparameter *texture-atlas-size* 1f0)
(defparameter *texture-cell-size* 16)

(defun face-direction-to-sunlight-mult (face-direction)
  (float
   (case face-direction
     (up 1.0f0)
     (top 1.0f0)

     (left 0.5f0)

     (right 0.75f0)

     (forward 0.8f0)
     (front 0.8f0)
     (ahead 0.8f0)

     (back 0.625f0)
     (backwards 0.625f0)
     
     (down 0.4f0)
     (bottom 0.4f0)
     
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
    (let ((sunlit-mult (face-direction-to-sunlight-mult 'front)))
      (list
       (list
        (list (3d-to-1d 0.0 1.0 0.0) (2d-to-1d 0.0 1.0) sunlit-mult)
        (list (3d-to-1d 0.0 0.0 0.0) (2d-to-1d 0.0 0.0) sunlit-mult)
        (list (3d-to-1d 1.0 0.0 0.0)  (2d-to-1d 1.0 1.0) sunlit-mult)
        (list (3d-to-1d 1.0 1.0 0.0)  (2d-to-1d 1.0 0.0) sunlit-mult))
       (list 2 1 0 3 2 0))))

  (defparameter cube-front
    (let ((sunlit-mult (face-direction-to-sunlight-mult 'back)))
      (list
       (list
        (list (3d-to-1d 0.0 1.0 1.0) (2d-to-1d 0.0 0.0) sunlit-mult)
        (list (3d-to-1d 1.0 1.0 1.0) (2d-to-1d 1.0 0.0) sunlit-mult)
        (list (3d-to-1d 1.0 0.0 1.0) (2d-to-1d 1.0 1.0) sunlit-mult)
        (list (3d-to-1d 0.0 0.0 1.0) (2d-to-1d 0.0 1.0) sunlit-mult))
       (list 2 1 0 3 2 0))))

  (defparameter cube-left 
    (let ((sunlit-mult (face-direction-to-sunlight-mult 'left)))
      (list
       (list
        (list (3d-to-1d 0.0 1.0 0.0) (2d-to-1d 0.0 0.0) sunlit-mult)
        (list (3d-to-1d 0.0 0.0 0.0) (2d-to-1d 0.0 1.0) sunlit-mult)
        (list (3d-to-1d 0.0 0.0 1.0) (2d-to-1d 1.0 1.0) sunlit-mult)
        (list (3d-to-1d 0.0 1.0 1.0) (2d-to-1d 1.0 0.0) sunlit-mult))
       (list 1 2 0 2 3 0))))

  (defparameter cube-right 
    (let ((sunlit-mult (face-direction-to-sunlight-mult 'right)))
      (list
       (list
        (list (3d-to-1d 1.0 1.0 0.0) (2d-to-1d 1.0 0.0) sunlit-mult)
        (list (3d-to-1d 1.0 1.0 1.0) (2d-to-1d 0.0 0.0) sunlit-mult)
        (list (3d-to-1d 1.0 0.0 0.0) (2d-to-1d 1.0 1.0) sunlit-mult)
        (list (3d-to-1d 1.0 0.0 1.0) (2d-to-1d 0.0 1.0) sunlit-mult))
       (list 3 2 0 1 3 0))))

  (defparameter cube-top 
    (let ((sunlit-mult (face-direction-to-sunlight-mult 'top)))
      (list
       (list
        (list (3d-to-1d 0.0 1.0 1.0) (2d-to-1d 0.0 1.0) sunlit-mult)
        (list (3d-to-1d 0.0 1.0 0.0) (2d-to-1d 0.0 0.0) sunlit-mult)
        (list (3d-to-1d 1.0 1.0 0.0) (2d-to-1d 1.0 0.0) sunlit-mult)
        (list (3d-to-1d 1.0 1.0 1.0) (2d-to-1d 1.0 1.0) sunlit-mult))
       (list 2 1 0 3 2 0))))

  (defparameter cube-bottom 
    (let ((sunlit-mult (face-direction-to-sunlight-mult 'bottom)))
      (list
       (list
        (list (3d-to-1d 0.0 0.0 1.0) (2d-to-1d 0.0 0.0) sunlit-mult)
        (list (3d-to-1d 1.0 0.0 0.0) (2d-to-1d 1.0 1.0) sunlit-mult)
        (list (3d-to-1d 0.0 0.0 0.0) (2d-to-1d 0.0 1.0) sunlit-mult)
        (list (3d-to-1d 1.0 0.0 1.0) (2d-to-1d 1.0 0.0) sunlit-mult))
       (list 2 1 0 1 3 0))))

  (let ((cache (make-hash-table :test #'equal)))
    (defun build-cube-mesh-from-faces (faces)
      (or (gethash faces cache)
          (setf (gethash faces cache) (combine-cube-faces (get-cube-faces faces)))))))

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
                                           
                                           1)))
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

