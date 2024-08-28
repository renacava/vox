(in-package #:vox)

(defun make-ivec2 (x y)
  (make-array 2 :element-type `(signed-byte 32) :initial-contents (vector x y)))

(defun make-ivec3 (x y z)
  (make-array 3 :element-type `(signed-byte 32) :initial-contents (vector x y z)))

(defun make-blocks-verts-and-indices-from-positions-and-symbols (positions-and-symbols)
  (let* ((solidity-array-result (multiple-value-list (make-chunk-block-solidity-array-from-positions-and-symbols positions-and-symbols)))
         (chunk-block-solidity-array (first solidity-array-result))
         (top-blocks-array2D (second solidity-array-result)))
    (setq my-top-blocks-array top-blocks-array2D)
    (combine-cube-faces
     (loop for pos-and-symb in positions-and-symbols
           collect (augment-cube-mesh-with-block-symbol-and-offset (make-cube-faces-from-adjacent-solids
                                                                    (coerce (subseq pos-and-symb 0 3) 'vector)
                                                                    chunk-block-solidity-array)
                                                                   (last1 pos-and-symb)
                                                                   (subseq pos-and-symb 0 3))))))

(defun combine-blocks-verts-and-indices (blocks-verts-and-indices)
  (let* ((vert-c-array  (make-c-array (first blocks-verts-and-indices) :element-type 'block-vert))
         (index-c-array (make-c-array (second blocks-verts-and-indices) :element-type :uint)))
    (list vert-c-array index-c-array)))
