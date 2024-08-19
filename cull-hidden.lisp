(in-package #:vox)

;; (defparameter *empty-chunk-block-array* (mak))

(defun cull-hidden-blocks-from-positions-and-symbols (positions-and-symbols)
  (let ((solid-p-table (get-solid-p-table-from-positions-and-symbols positions-and-symbols))
        (nil-chunk (loop for x below *chunk-width*
                         append (loop for y below *chunk-width*
                                      append (loop for z below *chunk-width*
                                                   collect (list x y z nil))))))
    nil-chunk
    )
  
  ;; sooo first grab each symbol's mesh's solid-p.
  ;; then populate a *chunk-width*^3 volume with nils.
  ;; then overwrite that volume with the positions-and-symbols at the right locations, so, foreach position-and-symbol in positions-and-symbols, setf volume x y z solidness to solid-p grabbed in step 1.
  ;; we now have a 3D volume of nils and t's, which we can use to check adjacent blocks for solidness. loop over it all and just return t or nil.
  ;; blocks on the chunk border get to stay by defualt for now.
  )

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
  (let ((block-array (make-empty-chunk-block-array)))  
    (loop for position-and-symbol in positions-and-symbols
          do (let ((index (3d-to-1d (first position-and-symbol)
                                    (second position-and-symbol)
                                    (third position-and-symbol))))
               (setf (aref block-array index) (get-symbol-mesh-solid-p (last1 position-and-symbol)))))
    block-array))

(defun make-cube-faces-from-adjacent-solids (index chunk-block-array)
  (build-cube-mesh-from-faces (remove-if-not #'identity
                                             (remove-if-not #'identity
                                                            (list (when (not (solid-above-p index chunk-block-array)) 'top)
                                                                  (when (not (solid-below-p index chunk-block-array)) 'bottom)
                                                                  (when (not (solid-right-p index chunk-block-array)) 'right)
                                                                  (when (not (solid-left-p index chunk-block-array)) 'left)
                                                                  (when (not (solid-ahead-p index chunk-block-array)) 'front)
                                                                  (when (not (solid-behind-p index chunk-block-array)) 'back)))))
  
  )

(defun index-above (index &optional (chunk-width *chunk-width*))
  (+ index chunk-width))

(defun index-below (index &optional (chunk-width *chunk-width*))
  (- index chunk-width))

(defun index-left (index)
  (- index 1))

(defun index-right (index)
  (+ index 1))

(defun index-ahead (index &optional (chunk-width *chunk-width*))
  (+ index (* chunk-width chunk-width)))

(defun index-behind (index &optional (chunk-width *chunk-width*))
  (- index (* chunk-width chunk-width)))

(defun index-solid-p (index chunk-block-array)
  (and (< -1 index *chunk-size*)
       (aref chunk-block-array index)))

(defun solid-above-p (index chunk-block-array &optional (chunk-width *chunk-width*))
  (index-solid-p (index-above index chunk-width) chunk-block-array))

(defun solid-below-p (index chunk-block-array &optional (chunk-width *chunk-width*))
  (index-solid-p (index-below index chunk-width) chunk-block-array))

(defun solid-left-p (index chunk-block-array)
  (index-solid-p (index-left index) chunk-block-array))

(defun solid-right-p (index chunk-block-array)
  (index-solid-p (index-right index) chunk-block-array))

(defun solid-ahead-p (index chunk-block-array &optional (chunk-width *chunk-width*))
  (index-solid-p (index-ahead index chunk-width) chunk-block-array))

(defun solid-behind-p (index chunk-block-array &optional (chunk-width *chunk-width*))
  (index-solid-p (index-behind index chunk-width) chunk-block-array))
