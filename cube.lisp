(in-package #:vox)

(defun make-blocks-verts-and-indices-from-positions-and-symbols (positions-and-symbols chunk-offset chunk-width)
  (let* ((solidity-array-result (multiple-value-list (make-chunk-block-solidity-array-from-positions-and-symbols positions-and-symbols chunk-width)))
         (chunk-block-solidity-array (first solidity-array-result))
         (top-blocks-array2D (second solidity-array-result))
         (chunk-offset (vector (float (first chunk-offset))
                               (float (second chunk-offset))
                               (float (third chunk-offset)))))
    (combine-cube-faces
     (loop for pos-and-symb in positions-and-symbols
           when (cadr pos-and-symb)
           collect (let* ((pos (car pos-and-symb))
                          (x (aref pos 0))
                          (y (aref pos 1))
                          (z (aref pos 2))
                          (block-symbol (cadr pos-and-symb)))
                     (augment-cube-mesh-with-block-symbol-and-offset
                      (make-cube-faces-from-adjacent-solids
                       x y z;;(vector x y z)
                       chunk-block-solidity-array
                       chunk-offset
                       chunk-width)
                      block-symbol 
                      pos;;(list x y z)
                      chunk-width
                      (- (aref top-blocks-array2D x z) y)
                      ;;(= (aref top-blocks-array2D x z) y)
                      ))))


    ;; (loop for pos-and-symb in positions-and-symbols
    ;;       when (last1 pos-and-symb)
    ;;       collect (let ((x (first pos-and-symb))
    ;;                     (y (second pos-and-symb))
    ;;                     (z (third pos-and-symb))
    ;;                     (block-symbol (last1 pos-and-symb)))
    ;;                 (augment-cube-mesh-with-block-symbol-and-offset
    ;;                  (make-cube-faces-from-adjacent-solids
    ;;                   (vector x y z)
    ;;                   chunk-block-solidity-array
    ;;                   (vec3 (float (first chunk-offset))
    ;;                         (float (second chunk-offset))
    ;;                         (float (third chunk-offset)))
    ;;                   chunk-width)
    ;;                  block-symbol 
    ;;                  (list x y z)
    ;;                  chunk-width
    ;;                  (- (aref top-blocks-array2D x z) y)
    ;;                  ;;(= (aref top-blocks-array2D x z) y)
    ;;                  )))

    ;; (loop for pos-and-symb in positions-and-symbols
    ;;       when (last1 pos-and-symb)
    ;;       collect (let ((x (float (first pos-and-symb)))
    ;;                     (y (float (second pos-and-symb)))
    ;;                     (z (float (third pos-and-symb)))
    ;;                     (block-symbol (last1 pos-and-symb)))
    ;;                 (make-cube-faces-from-adjacent-solids
    ;;                  (vec3 x y z)
    ;;                  chunk-block-solidity-array
    ;;                  (vec3 (float (first chunk-offset))
    ;;                        (float (second chunk-offset))
    ;;                        (float (third chunk-offset)))
    ;;                  chunk-width)))
    
    ))

(defun combine-blocks-verts-and-indices (blocks-verts-and-indices)
  (when (and blocks-verts-and-indices
             (car blocks-verts-and-indices)
             (cadr blocks-verts-and-indices))
    (let* ((vert-c-array  (make-c-array (first blocks-verts-and-indices) :element-type 'block-vert))
           (index-c-array (make-c-array (second blocks-verts-and-indices) :element-type :uint)))
      (list vert-c-array index-c-array)))
  )
