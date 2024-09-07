(in-package #:vox)

(defun get-solid-p-table-from-positions-and-symbols (positions-and-symbols)
  (let ((solid-p-table (make-hash-table)))
    (loop for position-and-symbol in positions-and-symbols
          do (let* ((symbol (last1 position-and-symbol)))
               (unless (gethash symbol solid-p-table)
                 (setf (gethash symbol solid-p-table)
                       (getf (get-mesh-bound-to-block-symbol symbol) :solid-p)))))
    solid-p-table))

(defun make-empty-chunk-block-array (chunk-width chunk-height)
  (declare (type fixnum chunk-width chunk-height)
           (optimize (speed 3) (safety 0)))
  (make-array (* chunk-width chunk-width chunk-height) :element-type 'bit :initial-element 0))

(defun make-chunk-block-solidity-array-from-positions-and-symbols (positions-and-symbols chunk-width chunk-height)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum chunk-width chunk-height))
  (let* ((block-array (make-empty-chunk-block-array chunk-width chunk-height))
         (xz-y-array (make-array (list chunk-width chunk-width) :element-type 'fixnum :initial-element 0)))  
    (declare (type (simple-array bit) block-array)
             (type (simple-array fixnum) xz-y-array))
    (loop for position-and-symbol in positions-and-symbols
          do (let* ((pos (car position-and-symbol))
                    (x (aref pos 0))
                    (y (aref pos 1))
                    (z (aref pos 2))
                    (block-symbol (cadr position-and-symbol))
                    (index (3d-to-1d-int x
                                         y
                                         z
                                         chunk-width
                                         chunk-height))
                    (solid? (get-symbol-mesh-solid-p block-symbol))
                    (y-array-entry (aref xz-y-array x z)))
               (declare (type (simple-array fixnum) pos)
                        (type fixnum index x y z index y-array-entry)
                        (type bit solid?))
               (if solid?
                   (if (= 0 y-array-entry)
                       (setf (aref xz-y-array x z) y)
                       (when (> y y-array-entry)
                         (setf (aref xz-y-array x z) y))))
               (setf (aref block-array index) (if solid? 1 0))))
    (values block-array xz-y-array)))

(progn
  (defmacro define-solid-func (name x y z)
   `(defun ,name (x y z chunk-block-array chunk-offset-x chunk-offset-y chunk-offset-z chunk-width chunk-height)
      (declare (type fixnum x y z chunk-offset-x chunk-offset-y chunk-offset-z)
               (type unsigned-byte chunk-width chunk-height)
               (optimize (speed 3) (safety 0)))
      (pos-solid ,x ,y ,z chunk-block-array chunk-offset-x chunk-offset-y chunk-offset-z chunk-width chunk-height)))


  (define-solid-func solid-above-p x (1+ y) z)
  (define-solid-func solid-below-p x (1- y) z)
  (define-solid-func solid-left-p (1- x) y z)
  (define-solid-func solid-right-p (1+ x) y z)
  (define-solid-func solid-ahead-p x y (1+ z))
  (define-solid-func solid-behind-p x y (1- z)))

(defun make-cube-faces-from-adjacent-solids (x y z chunk-block-array chunk-offset-x chunk-offset-y chunk-offset-z chunk-width chunk-height)
  (declare (type fixnum x y z chunk-width chunk-height chunk-offset-x chunk-offset-y chunk-offset-z)
           (type (simple-array bit) chunk-block-array)
           (optimize (speed 3) (safety 0)))
  (flet ((query-pos (solidity-func face)
           (when (not (funcall solidity-func x y z chunk-block-array chunk-offset-x chunk-offset-y chunk-offset-z chunk-width chunk-height))
             face)))
    (combine-cube-faces
     (loop for face across (vector (query-pos #'solid-above-p cube-top)
                                   (query-pos #'solid-below-p cube-bottom)
                                   (query-pos #'solid-right-p cube-right)
                                   (query-pos #'solid-left-p cube-left)
                                   (query-pos #'solid-ahead-p cube-front)
                                   (query-pos #'solid-behind-p cube-back))
           when face
           collect face))))


(defparameter interchunk-culling? t)

(defun pos-solid (x y z chunk-block-array chunk-offset-x chunk-offset-y chunk-offset-z chunk-width chunk-height)
  (declare (type fixnum x y z chunk-width chunk-height chunk-offset-x chunk-offset-y chunk-offset-z)
           (type (simple-array bit) chunk-block-array)
           (optimize (speed 3) (safety 3) (debug 3)))
  (when (< y 0)
    (return-from pos-solid t))
  (let* ((border-pos? (and interchunk-culling?
                           (or (= x -1)
                               (= z -1)
                               (= x chunk-width)
                               (= z chunk-width))))
         (offset-pos (when border-pos?
                       (vector (+ x (* chunk-offset-x chunk-width))
                               y
                               (+ z (* chunk-offset-z chunk-width)))
                       ))
         (border-pos-block (when offset-pos
                             (vws:sample-single-pos offset-pos chunk-width chunk-height)))
         (border-block-solidity (when border-pos-block
                                  (get-symbol-mesh-solid-p border-pos-block)))
         )
    (if border-pos?
        border-block-solidity
        (and (< -1 x chunk-width)
             (< -1 y chunk-height)
             (< -1 z chunk-width)
             (= 1 (aref chunk-block-array (3d-to-1d-int x y z chunk-width chunk-height)))))))


