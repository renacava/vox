(in-package #:vox)

(defun make-ivec2 (x y)
  (make-array 2 :element-type `(signed-byte 32) :initial-contents (vector x y)))

(defun make-ivec3 (x y z)
  (make-array 3 :element-type `(signed-byte 32) :initial-contents (vector x y z)))

(defun make-blocks-verts-and-indices-from-positions-and-symbols (positions-and-symbols)
  (let* ((solidity-array-result (multiple-value-list (make-chunk-block-solidity-array-from-positions-and-symbols positions-and-symbols)))
         (chunk-block-solidity-array (first solidity-array-result))
         (top-blocks-array2D (second solidity-array-result)))
    (combine-cube-faces
     (loop for pos-and-symb in positions-and-symbols
           collect (let ((x (first pos-and-symb))
                         (y (second pos-and-symb))
                         (z (third pos-and-symb))
                         (block-symbol (last1 pos-and-symb)))
                     (augment-cube-mesh-with-block-symbol-and-offset
                      (make-cube-faces-from-adjacent-solids
                       (vector x y z)
                       chunk-block-solidity-array)
                      block-symbol
                      (list x y z)
                      (= (aref top-blocks-array2D x z) y)))))))

(defun combine-blocks-verts-and-indices (blocks-verts-and-indices)
  (let* ((vert-c-array  (make-c-array (first blocks-verts-and-indices) :element-type 'block-vert))
         (index-c-array (make-c-array (second blocks-verts-and-indices) :element-type :uint)))
    (list vert-c-array index-c-array)))
