(in-package #:vox)

(defun get-solid-p-table-from-positions-and-symbols (positions-and-symbols)
  (let ((solid-p-table (make-hash-table)))
    (loop for position-and-symbol in positions-and-symbols
          do (let* ((symbol (last1 position-and-symbol)))
               (unless (gethash symbol solid-p-table)
                 (setf (gethash symbol solid-p-table)
                       (getf (get-mesh-bound-to-block-symbol symbol) :solid-p)))))
    solid-p-table))

(defun make-empty-chunk-block-array (chunk-width chunk-height)
  (make-array (* chunk-width chunk-width chunk-height) :initial-element nil))

(defun make-chunk-block-solidity-array-from-positions-and-symbols (positions-and-symbols chunk-width &optional (chunk-height *chunk-height*))
  (let* ((block-array (make-empty-chunk-block-array chunk-width chunk-height))
         (xz-y-array (make-array (list chunk-width chunk-width) :initial-element nil)))  
    (loop for position-and-symbol in positions-and-symbols
          do (let* ((x (first position-and-symbol))
                    (y (second position-and-symbol))
                    (z (third position-and-symbol))
                    (block-symbol (last1 position-and-symbol))
                    (index (truncate (3d-to-1d x
                                      y
                                      z
                                      chunk-width
                                      chunk-height)))
                    (solid? (get-symbol-mesh-solid-p block-symbol))
                    (y-array-entry (aref xz-y-array x z)))
               (if solid?
                   (if y-array-entry
                       (when (> y y-array-entry)
                         (setf (aref xz-y-array x z) y))
                       (setf (aref xz-y-array x z) y)))
               (setf (aref block-array index) solid?)))
    (values block-array xz-y-array)))


(defun make-cube-faces-from-adjacent-solids (pos chunk-block-array chunk-offset chunk-width)
  (flet ((query-pos (solidity-func face)
           (when (not (funcall solidity-func pos chunk-block-array chunk-offset chunk-width)) face)))
    (build-cube-mesh-from-faces
     ;; (remove-if-not #'identity
     ;;                (remove-if-not #'identity
     ;;                               (setq my-data (list (query-pos #'solid-above-p 'top)
     ;;                                                   (query-pos #'solid-below-p 'bottom)
     ;;                                                   (query-pos #'solid-right-p 'right)
     ;;                                                   (query-pos #'solid-left-p 'left)
     ;;                                                   (query-pos #'solid-ahead-p 'front)
     ;;                                                   (query-pos #'solid-behind-p 'back)
     ;;                                                   ))))
     (list (query-pos #'solid-above-p 'top)
           (query-pos #'solid-below-p 'bottom)
           (query-pos #'solid-right-p 'right)
           (query-pos #'solid-left-p 'left)
           (query-pos #'solid-ahead-p 'front)
           (query-pos #'solid-behind-p 'back)
           )
     )
    
    ;; (list (query-pos #'solid-above-p 'top)
    ;;       (query-pos #'solid-below-p 'bottom)
    ;;       (query-pos #'solid-right-p 'right)
    ;;       (query-pos #'solid-left-p 'left)
    ;;       (query-pos #'solid-ahead-p 'front)
    ;;       (query-pos #'solid-behind-p 'back)
    ;;       )
    ))

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

(defparameter interchunk-culling? t)

(defun pos-solid (pos chunk-block-array chunk-offset chunk-width)
  (when (< (aref pos 1) 0)
    (return-from pos-solid t))
  (let* ((x (aref pos 0))
         (y (aref pos 1))
         (z (aref pos 2))
         (border-pos? (and interchunk-culling?
                           (or (= x -1)
                               (= z -1)
                               (= x chunk-width)
                               (= z chunk-width))))
         (offset-pos (when border-pos?
                       (v3:+
                        (vec3 (float x)
                              (float y)
                              (float z))
                        (vec3 (* (aref chunk-offset 0) chunk-width)
                              0.0
                              (* (aref chunk-offset 2) chunk-width)))))
         (border-pos-block (when offset-pos
                             (vws:sample-single-pos offset-pos chunk-width *chunk-height*)))
         (border-block-solidity (when border-pos-block
                                  (get-symbol-mesh-solid-p border-pos-block)))
         )
    (if border-pos?
        border-block-solidity
        (and (< -1 x chunk-width)
             (< -1 y *chunk-height*)
             (< -1 z chunk-width)
             (aref chunk-block-array (truncate (3d-to-1d x y z chunk-width *chunk-height*))))))
  )

(defun solid-above-p (pos chunk-block-array chunk-offset chunk-width)
  (pos-solid (pos-above pos) chunk-block-array chunk-offset chunk-width)
  ;;(pos-solid (v3:+ pos (vec3 0.0 1.0 0.0)) chunk-block-array chunk-offset chunk-width)
  )

(defun solid-below-p (index chunk-block-array chunk-offset chunk-width)
  (pos-solid (pos-below index) chunk-block-array chunk-offset chunk-width))

(defun solid-left-p (index chunk-block-array chunk-offset chunk-width)
  (pos-solid (pos-left index) chunk-block-array chunk-offset chunk-width))

(defun solid-right-p (index chunk-block-array chunk-offset chunk-width)
  (pos-solid (pos-right index) chunk-block-array chunk-offset chunk-width))

(defun solid-ahead-p (index chunk-block-array chunk-offset chunk-width)
  (pos-solid (pos-ahead index) chunk-block-array chunk-offset chunk-width))

(defun solid-behind-p (index chunk-block-array chunk-offset chunk-width)
  (pos-solid (pos-behind index) chunk-block-array chunk-offset chunk-width))
