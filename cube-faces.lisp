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
        (list (3d-to-1d 0.0 1.0 0.0 2 2) (2d-to-1d 0.0 1.0 2) face-float)
        (list (3d-to-1d 0.0 0.0 0.0 2 2) (2d-to-1d 0.0 0.0 2) face-float)
        (list (3d-to-1d 1.0 0.0 0.0 2 2)  (2d-to-1d 1.0 1.0 2) face-float)
        (list (3d-to-1d 1.0 1.0 0.0 2 2)  (2d-to-1d 1.0 0.0 2) face-float))
       (list 2 1 0 3 2 0))))

  (defparameter cube-front
    (let ((face-float (face-direction-to-float 'back)))
      (list
       (list
        (list (3d-to-1d 0.0 1.0 1.0 2 2) (2d-to-1d 0.0 0.0 2) face-float)
        (list (3d-to-1d 1.0 1.0 1.0 2 2) (2d-to-1d 1.0 0.0 2) face-float)
        (list (3d-to-1d 1.0 0.0 1.0 2 2) (2d-to-1d 1.0 1.0 2) face-float)
        (list (3d-to-1d 0.0 0.0 1.0 2 2) (2d-to-1d 0.0 1.0 2) face-float))
       (list 2 1 0 3 2 0))))

  (defparameter cube-left 
    (let ((face-float (face-direction-to-float 'left)))
      (list
       (list
        (list (3d-to-1d 0.0 1.0 0.0 2 2) (2d-to-1d 0.0 0.0 2) face-float)
        (list (3d-to-1d 0.0 0.0 0.0 2 2) (2d-to-1d 0.0 1.0 2) face-float)
        (list (3d-to-1d 0.0 0.0 1.0 2 2) (2d-to-1d 1.0 1.0 2) face-float)
        (list (3d-to-1d 0.0 1.0 1.0 2 2) (2d-to-1d 1.0 0.0 2) face-float))
       (list 1 2 0 2 3 0))))

  (defparameter cube-right 
    (let ((face-float (face-direction-to-float 'right)))
      (list
       (list
        (list (3d-to-1d 1.0 1.0 0.0 2 2) (2d-to-1d 1.0 0.0 2) face-float)
        (list (3d-to-1d 1.0 1.0 1.0 2 2) (2d-to-1d 0.0 0.0 2) face-float)
        (list (3d-to-1d 1.0 0.0 0.0 2 2) (2d-to-1d 1.0 1.0 2) face-float)
        (list (3d-to-1d 1.0 0.0 1.0 2 2) (2d-to-1d 0.0 1.0 2) face-float))
       (list 3 2 0 1 3 0))))

  (defparameter cube-top 
    (let ((face-float (face-direction-to-float 'top)))
      (list
       (list
        (list (3d-to-1d 0.0 1.0 1.0 2 2) (2d-to-1d 0.0 1.0 2) face-float)
        (list (3d-to-1d 0.0 1.0 0.0 2 2) (2d-to-1d 0.0 0.0 2) face-float)
        (list (3d-to-1d 1.0 1.0 0.0 2 2) (2d-to-1d 1.0 0.0 2) face-float)
        (list (3d-to-1d 1.0 1.0 1.0 2 2) (2d-to-1d 1.0 1.0 2) face-float))
       (list 2 1 0 3 2 0))))

  (defparameter cube-bottom 
    (let ((face-float (face-direction-to-float 'bottom)))
      (list
       (list
        (list (3d-to-1d 0.0 0.0 1.0 2 2) (2d-to-1d 0.0 0.0 2) face-float)
        (list (3d-to-1d 1.0 0.0 0.0 2 2) (2d-to-1d 1.0 1.0 2) face-float)
        (list (3d-to-1d 0.0 0.0 0.0 2 2) (2d-to-1d 0.0 1.0 2) face-float)
        (list (3d-to-1d 1.0 0.0 1.0 2 2) (2d-to-1d 1.0 0.0 2) face-float))
       (list 2 1 0 1 3 0))))

  (let ((cache (make-hash-table :test #'equal)))
    (defun build-cube-mesh-from-faces (faces)
      (or (gethash faces cache)
          (setf (gethash faces cache) (combine-cube-faces (get-cube-faces (remove-duplicates faces))))))))

(defun augment-cube-mesh-with-block-symbol-and-offset (cube-mesh block-symbol offset &optional (distance-from-top-block 0))
  (let ((verts (first cube-mesh))
        (mesh-instance (get-mesh-bound-to-block-symbol block-symbol)))
    (list (loop for vert in verts
                collect (let* ((face-float (third vert))
                               (face-float
                                 (+ face-float
                                    (case distance-from-top-block
                                      (0 18f0)
                                      (1 12f0)
                                      (2 12f0)
                                      (3 12f0)
                                      (4 12f0)
                                      (5 6f0)
                                      (6 6f0)
                                      (7 6f0)
                                      (8 6f0)
                                      (t 0f0)))
                                 
                                 ;;(if highest-block-in-chunk? (+ 6f0 face-float) face-float)
                                 ))
                          
                          (list
                           (encode-vert-data1 (first vert)
                                              (second vert)
                                              (3d-to-1d (float (first offset))
                                                        (float (second offset))
                                                        (float (third offset))))
                           (encode-vert-data2 face-float
                                              (2d-to-1d (getf mesh-instance :atlas-column)
                                                        (getf mesh-instance :atlas-row)
                                                        256)))
                          ;; (append pos-and-uv
                          ;;         (list
                          ;;          face-float
                          ;;          (2d-to-1d (getf mesh-instance :atlas-column)
                          ;;                    (getf mesh-instance :atlas-row))
                          ;;          (3d-to-1d (float (first offset))
                          ;;                    (float (second offset))
                          ;;                    (float (third offset)))))
                          ))
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
 
(defun pad-int-left-to-str (int-to-pad desired-length)
  (let* ((int-str (format nil "~a" int-to-pad))
         (int-lst (coerce int-str 'list)))
    (coerce
     (left-pad int-lst :desired-length desired-length :padding #\0)
     'string)))



(defun 1d-to-3dc (index cols depth)
  (let* ((z (truncate (/ index (* cols depth))))
         (index (- index (* z cols depth)))
         (x (mod index cols))
         (y (truncate (/ index cols))))
    (vec3 (float x) (float y) (float z))))

(defun 1d-to-2dc (index cols)
  (let* ((x (mod index cols))
         (y (truncate (/ index cols))))
    (vec2 (float x) (float y))))

(defun decode-vert-data (vert-int)
  (let* ((vert (format nil "~a" vert-int))
         (pos (parse-integer (to-string (char vert 1))))
         (uv (parse-integer (to-string (char vert 2))))
         (face-float (parse-integer (subseq vert 3 5)))
         (texture-atlas-index (parse-integer (subseq vert 5 10))))
    (list :pos (1d-to-3dc pos 2 2)
          :uv (1d-to-2dc uv 2)
          :face-float face-float
          :texture-atlas-index (1d-to-2dc texture-atlas-index 128))))

(defun decode-vert-data-nostrings (vert-int)
  (let* ((vert (- vert-int 1000000000))
         (pos (truncate (/ vert 100000000)))
         (vert (- vert (* pos 100000000)))

         (uv (truncate (/ vert 10000000)))
         (vert (- vert (* uv 10000000)))
         (uv (1d-to-2dc uv 2))
         (face-float (float (truncate (/ vert 100000))))
         (vert (- vert (* face-float 100000)))

         (texture-atlas-index (1d-to-2dc vert 128)))
    (list :pos (1d-to-3dc pos 2 2)
          :uv uv
          :face-float face-float
          :texture-atlas-index texture-atlas-index)))

(defun decode-test(vert-int)
  (let* ((vert-int (truncate vert-int))
         (vert (- vert-int 1000000000))
         (pos (truncate (/ vert 100000000)))
         (vert (- vert (* pos 100000000)))
         (uv (truncate (/ vert 10000000)))
         (vert (- vert (* uv 10000000)))
         (face-float (float (truncate (/ vert 100000)))))
    (list :pos pos
          :uv uv
          :face-float face-float)))

(defun decode-vert-float (vert-float)
  (let* ((vert-float (float vert-float))
         (pos (floor (* 10 vert-float)))
         (vert-float (* 10 vert-float))
         
         )
    (list :pos pos
          :vert vert-float
          ;; :uv uv
          ;; :face-float face-float
          ;; :texture-atlas-index texture-atlas-index
          )))

(defun encode-vert-data (pos-index uv-index face-light-float)
  (let ((face-light-float (pad-int-left-to-str (truncate face-light-float) 2))
        ;;(texture-atlas-index (pad-int-left-to-str (truncate texture-atlas-index) 5))
        )
    (read-from-string
     (format nil
             "1~af0"
             (pad-int-left-to-str
              (format nil "~a~a~a"
                      (truncate pos-index)
                      (truncate uv-index)
                      face-light-float)
              4)))))

(defun decode-test-shortint (vert-int)
  (let* ((vert-int (truncate vert-int))
         (vert (- vert-int 10000))
         (pos (truncate (/ vert 1000)))
         (vert (- vert (* pos 1000)))
         
         (uv (truncate (/ vert 100)))
         (vert (- vert (* uv 100)))
         (uv (1d-to-2dc uv 2))
         (face-float (float vert))
         )
    (list :pos (1d-to-3dc pos 2 2)
          :uv uv
          :face-float face-float)))

(defun encode-vert-data1 (pos-index uv-index local-offset-index)
  (let ((local-offset-index (pad-int-left-to-str (truncate local-offset-index) 6)))
    (read-from-string
     (format nil "~a.~a~af0"
             local-offset-index
             (truncate pos-index)
             (truncate uv-index)
))))

(defun encode-vert-data2 (face-light-float texture-atlas-index)
  (let ((face-light-float (pad-int-left-to-str (truncate face-light-float) 2))
        (texture-atlas-index (pad-int-left-to-str (truncate texture-atlas-index) 5)))
    (read-from-string
     (format nil "~a.~af0"
             face-light-float
             texture-atlas-index))))

(defun decode-test-shortint1 (vert-int)
  (let* ((local-offset-index (floor vert-int))
         (vert-int (* vert-int 100))
         (vert-int (- vert-int (* local-offset-index 100)))
         (pos (floor (/ vert-int 10)))
         (uv (- vert-int (* 10 pos)))
         ;;(texture-atlas-index (* vert-int 100000))
         ;;(texture-atlas-index (- texture-atlas-index (* 100000 face-float)))
         )
    (list :pos (1d-to-3dc pos 2 2)
          :uv (1d-to-2dc uv 2)
          :local-offset-index (1d-to-3dc local-offset-index 16 128))))

(defun decode-test-shortint2 (vert-int)
  (let* ((face-float (floor vert-int))
         (texture-atlas-index (* vert-int 100000))
         (texture-atlas-index (- texture-atlas-index (* 100000 face-float))))
    (list :face-float face-float
          :texture-atlas-index (1d-to-2dc texture-atlas-index 256))))
