(in-package #:vox)

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
