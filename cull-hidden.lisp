(in-package #:vox)

(defun get-solid-p-table-from-positions-and-symbols (positions-and-symbols)
  (let ((solid-p-table (make-hash-table)))
    (loop for position-and-symbol in positions-and-symbols
          do (let* ((symbol (last1 position-and-symbol)))
               (unless (gethash symbol solid-p-table)
                 (setf (gethash symbol solid-p-table)
                       (getf (get-mesh-bound-to-block-symbol symbol) :solid-p)))))
    solid-p-table))

(defun make-empty-chunk-block-array ()
  (make-array *chunk-size* :initial-element nil))

(defun make-chunk-block-solidity-array-from-positions-and-symbols (positions-and-symbols)
  (let ((block-array (make-empty-chunk-block-array))
        (highest-block 0))  
    (loop for position-and-symbol in positions-and-symbols
          do (let ((index (3d-to-1d (first position-and-symbol)
                                    (second position-and-symbol)
                                    (third position-and-symbol)))
                   (solid? (get-symbol-mesh-solid-p (last1 position-and-symbol))))
               (when (and solid?
                          (> (second position-and-symbol) highest-block))
                 (setf highest-block (second position-and-symbol)))
               (setf (aref block-array index) solid?)))
    block-array))

(defun make-cube-faces-from-adjacent-solids (pos chunk-block-array)
  (build-cube-mesh-from-faces (remove-if-not #'identity
                                             (remove-if-not #'identity
                                                            (list (when (not (solid-above-p pos chunk-block-array)) 'top)
                                                                  (when (not (solid-below-p pos chunk-block-array)) 'bottom)
                                                                  (when (not (solid-right-p pos chunk-block-array)) 'right)
                                                                  (when (not (solid-left-p pos chunk-block-array)) 'left)
                                                                  (when (not (solid-ahead-p pos chunk-block-array)) 'front)
                                                                  (when (not (solid-behind-p pos chunk-block-array)) 'back))))))

(defun pos-above (pos)
  (vector (aref pos 0)
          (1+ (aref pos 1))
          (aref pos 2)))

(defun pos-below (pos)
  (vector (aref pos 0)
          (1- (aref pos 1))
          (aref pos 2)))

(defun pos-ahead (pos)
  (vector (aref pos 0)
          (aref pos 1)
          (1+ (aref pos 2))))

(defun pos-behind (pos)
  (vector (aref pos 0)
          (aref pos 1)
          (1- (aref pos 2))))

(defun pos-left (pos)
  (vector (1- (aref pos 0))
          (aref pos 1)
          (aref pos 2)))

(defun pos-right (pos)
  (vector (1+ (aref pos 0))
          (aref pos 1)
          (aref pos 2)))

(defun pos-solid (pos chunk-block-array)
  (let ((x (aref pos 0))
        (y (aref pos 1))
        (z (aref pos 2)))
    (and (< -1 x *chunk-width*)
         (< -1 y *chunk-height*)
         (< -1 z *chunk-width*)
         (aref chunk-block-array (3d-to-1d x y z)))))

(defun solid-above-p (pos chunk-block-array)
  (pos-solid (pos-above pos) chunk-block-array))

(defun solid-below-p (index chunk-block-array)
  (pos-solid (pos-below index) chunk-block-array))

(defun solid-left-p (index chunk-block-array)
  (pos-solid (pos-left index) chunk-block-array))

(defun solid-right-p (index chunk-block-array)
  (pos-solid (pos-right index) chunk-block-array))

(defun solid-ahead-p (index chunk-block-array)
  (pos-solid (pos-ahead index) chunk-block-array))

(defun solid-behind-p (index chunk-block-array)
  (pos-solid (pos-behind index) chunk-block-array))
