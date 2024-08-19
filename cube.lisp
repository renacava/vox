(in-package #:vox)

(defun make-ivec2 (x y)
  (make-array 2 :element-type `(signed-byte 32) :initial-contents (vector x y)))

(defun make-ivec3 (x y z)
  (make-array 3 :element-type `(signed-byte 32) :initial-contents (vector x y z)))

(defun make-block-verts-and-indices (offset &optional (index-offset 0) (block-symbol nil))
  (declare (optimize (speed 3) (safety 3))
           (type fixnum index-offset))
  (let ((mesh

          (get-mesh-bound-to-block-symbol block-symbol)))
    (setf index-offset (* index-offset (getf mesh :n-verts)))
    (list (mapcar (lambda (vert)
                    (append vert (list (2d-to-1d (getf mesh :atlas-column)
                                                 (getf mesh :atlas-row))
                                       (3d-to-1d (float (first offset))
                                                 (float (second offset))
                                                 (float (third offset))))))
                  (getf mesh :verts))
          (loop for index fixnum in (getf mesh :indices)
                collect (+ index index-offset)))))

(defun make-blocks-verts-and-indices-from-positions-and-symbols (positions-and-symbols)
  (let (;;(index-offset -1)
        (chunk-block-solidity-array (make-chunk-block-solidity-array-from-positions-and-symbols positions-and-symbols))
        )
    ;; (loop for pos-and-symb in positions-and-symbols
    ;;       collect (make-block-verts-and-indices (subseq pos-and-symb 0 3)
    ;;                                             (incf index-offset)
    ;;                                             (last1 pos-and-symb)))
    (combine-cube-faces
     (loop for pos-and-symb in positions-and-symbols
           collect (augment-cube-mesh-with-block-symbol-and-offset (make-cube-faces-from-adjacent-solids
                                                                    (3d-to-1d (first pos-and-symb)
                                                                              (second pos-and-symb)
                                                                              (third pos-and-symb))
                                                                    chunk-block-solidity-array)
                                                                   (last1 pos-and-symb)
                                                                   (subseq pos-and-symb 0 3))))
    ))

(defun combine-blocks-verts-and-indices (blocks-verts-and-indices)
  (let* (;;(vert-vecs (mapcar #'first blocks-verts-and-indices))
         ;;(index-lists (mapcar #'second blocks-verts-and-indices))
         ;;(verts (loop for vert-list in vert-vecs append vert-list))
         ;;(indices (apply #'concatenate 'list index-lists))
         (vert-c-array  (make-c-array (first blocks-verts-and-indices) :element-type 'block-vert))
         (index-c-array (make-c-array (second blocks-verts-and-indices) :element-type :uint)))
    (list vert-c-array index-c-array)))
