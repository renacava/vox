(in-package #:vox)

(defun make-blocks-verts-and-indices-from-positions-and-symbols (positions-and-symbols chunk-offset-x chunk-offset-y chunk-offset-z chunk-width chunk-height chunk-offset-vec3)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum chunk-offset-x chunk-offset-y chunk-offset-z chunk-width chunk-height))
  (let* ((solidity-array-result (multiple-value-list (make-chunk-block-solidity-array-from-positions-and-symbols positions-and-symbols chunk-width chunk-height)))
         (chunk-block-solidity-array (first solidity-array-result))
         (top-blocks-array2D (second solidity-array-result))
         )
    (declare (type (simple-array bit) chunk-block-solidity-array)
             (type (simple-array fixnum) top-blocks-array2d)
             )
    (combine-mesh-faces
     (loop for pos-and-symb in positions-and-symbols
           when (cadr pos-and-symb)
           collect (let* ((pos (car pos-and-symb))
                          (x (aref pos 0))
                          (y (aref pos 1))
                          (z (aref pos 2))
                          (block-symbol (cadr pos-and-symb)))
                     (declare (type (simple-array fixnum) pos)
                              (type fixnum x y z))
                     (augment-cube-mesh-with-block-symbol-and-offset
                      (make-cube-faces-from-adjacent-solids
                       x y z
                       chunk-block-solidity-array
                       chunk-offset-x
                       chunk-offset-y
                       chunk-offset-z
                       chunk-width
                       chunk-height)
                      block-symbol 
                      pos
                      chunk-width
                      (- (aref top-blocks-array2D x z) y)
                      chunk-offset-vec3
                      ;;(= (aref top-blocks-array2D x z) y)
                      ))))


    ;; (loop for pos-and-symb in positions-and-symbols
    ;;       when (cadr pos-and-symb)
    ;;       collect (let* ((pos (car pos-and-symb))
    ;;                      (x (aref pos 0))
    ;;                      (y (aref pos 1))
    ;;                      (z (aref pos 2))
    ;;                      (block-symbol (cadr pos-and-symb)))
    ;;                 (augment-cube-mesh-with-block-symbol-and-offset
    ;;                  (make-cube-faces-from-adjacent-solids
    ;;                   x y z
    ;;                   chunk-block-solidity-array
    ;;                   chunk-offset-x
    ;;                   chunk-offset-y
    ;;                   chunk-offset-z
    ;;                   chunk-width
    ;;                   chunk-height)
    ;;                  block-symbol 
    ;;                  pos
    ;;                  chunk-width
    ;;                  (- (aref top-blocks-array2D x z) y)
    ;;                  ;;(= (aref top-blocks-array2D x z) y)
    ;;                  )))
    
    
    ))

(defun combine-blocks-verts-and-indices (blocks-verts-and-indices)
  (when (and blocks-verts-and-indices
             (car blocks-verts-and-indices)
             (cadr blocks-verts-and-indices))
    (let* ((vert-c-array  (make-c-array (first blocks-verts-and-indices) :element-type 'block-vert))
           (index-c-array (make-c-array (second blocks-verts-and-indices) :element-type :uint)))
      (list vert-c-array index-c-array)))
  )
