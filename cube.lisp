(in-package #:vox)

(defun make-blocks-verts-and-indices-from-positions-and-symbols (positions-and-symbols chunk-offset)
  (let* ((solidity-array-result (multiple-value-list (make-chunk-block-solidity-array-from-positions-and-symbols positions-and-symbols)))
         (chunk-block-solidity-array (first solidity-array-result))
         (top-blocks-array2D (second solidity-array-result)))
    (combine-cube-faces
     (loop for pos-and-symb in positions-and-symbols
           when (last1 pos-and-symb)
           collect (let ((x (first pos-and-symb))
                         (y (second pos-and-symb))
                         (z (third pos-and-symb))
                         (block-symbol (last1 pos-and-symb)))
                     (augment-cube-mesh-with-block-symbol-and-offset
                      (make-cube-faces-from-adjacent-solids
                       (vector x y z)
                       chunk-block-solidity-array
                       (vec3 (float (first chunk-offset))
                             (float (second chunk-offset))
                             (float (third chunk-offset))))
                      block-symbol
                      (list x y z)
                      (- (aref top-blocks-array2D x z) y)
                      ;;(= (aref top-blocks-array2D x z) y)
                      ))))))

(defun combine-blocks-verts-and-indices (blocks-verts-and-indices)
  (let* ((vert-c-array  (make-c-array (first blocks-verts-and-indices) :element-type 'block-vert))
         (index-c-array (make-c-array (second blocks-verts-and-indices) :element-type :uint)))
    (list vert-c-array index-c-array)))
