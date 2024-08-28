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

;; make a hash-table where each key:value is `(x z):highets-block-found.
;; you would need to be doing a bunch of gethash operations... or maybe we could do a 2D array? do the array if performance becomes an issue.
;; this is happening in various threads so its okay if it isn't the fastest.
;; anyway.
;; make a hash table using xz as keys and height as vlaues. then foreach pos-symbol in positinos-and-symbols, grab the value at key, if it exists then check if y is greater than value.
;; if it IS greater than the current value, set its new value to the current Y value we're looking at.
;; if it doesn't exist then just set the current value to current y as well, setf gethash.
;; it it exists but its y > current-y then do nothing.

(defun make-chunk-block-solidity-array-from-positions-and-symbols (positions-and-symbols)
  (let ((block-array (make-empty-chunk-block-array))
        (xz-y-array (make-array (list *chunk-width* *chunk-width*) :initial-element nil)))  
    (loop for position-and-symbol in positions-and-symbols
          do (let* ((x (first position-and-symbol))
                    (y (second position-and-symbol))
                    (z (third position-and-symbol))
                    (block-symbol (last1 position-and-symbol))
                    (index (3d-to-1d x
                                     y
                                     z))
                    (solid? (get-symbol-mesh-solid-p block-symbol))
                    (y-array-entry (aref xz-y-array x z)))
               (if solid?
                   (if y-array-entry
                       (when (> y y-array-entry)
                         (setf (aref xz-y-array x z) y))
                       (setf (aref xz-y-array x z) y)))
               (setf (aref block-array index) solid?)))
    (values block-array xz-y-array)))

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
