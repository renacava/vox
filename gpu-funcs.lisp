(in-package #:vox)


(defstruct-g ssbo-struct
  (data (:vec4 (2304)) :accessor data)) ;; (* 48 48) => 2304; the size in pixels of the texture atlas.

(defun-g 2d-to-1d-g ((x :int) (y :int) (array-width :int))
  (+ x (* y array-width)))

(defun-g face-light-float-to-multiplier ((face-light-float :float))
  (labels ((fleq ((x :int) (subsequent :float) (alternative :float))
             (float-eq-or face-light-float x subsequent alternative)))

    (*
     (fleq 0   0.53   ;; TOP     UN-SUNLIT
     (fleq 1   0.5    ;; LEFT    UN-SUNLIT
     (fleq 2   0.5    ;; RIGHT   UN-SUNLIT
     (fleq 3   0.4    ;; FRONT   UN-SUNLIT
     (fleq 4   0.4    ;; BACK    UN-SUNLIT
     (fleq 5   0.32   ;; BOTTOM  UN-SUNLIT 
           
     (fleq 6   0.8    ;; TOP     SUNLIT
     (fleq 7   0.6    ;; LEFT    SUNLIT
     (fleq 8   0.6    ;; RIGHT   SUNLIT
     (fleq 9   0.5    ;; FRONT   SUNLIT
     (fleq 10  0.5    ;; BACK    SUNLIT
     (fleq 11  0.4    ;; BOTTOM  SUNLIT

     (fleq 12  1.0    ;; TOP     SUNLIT
     (fleq 13  0.7    ;; LEFT    SUNLIT
     (fleq 14  0.7    ;; RIGHT   SUNLIT
     (fleq 15  0.6    ;; FRONT   SUNLIT
     (fleq 16  0.6    ;; BACK    SUNLIT
     (fleq 17  0.5    ;; BOTTOM  SUNLIT

     (fleq 18  1.0    ;; TOP     SUNLIT
     (fleq 19  0.85   ;; LEFT    SUNLIT
     (fleq 20  0.85   ;; RIGHT   SUNLIT
     (fleq 21  0.75   ;; FRONT   SUNLIT
     (fleq 22  0.75   ;; BACK    SUNLIT
     (fleq 23  0.6    ;; BOTTOM  SUNLIT
         
               0.0)   ;; DEFAULT 
     )))))))))))))))))))))))
     
     (if (> face-light-float 5)
         1.0
         1.1))))

(defun-g my-cool-perlin-noise ((x :float))
  (nineveh:perlin-noise (vec2 x x)))

(defun-g muller ((x :float) (y :float))
  (*
   (sqrt (* -2 (log x)))
   (cos (float (* 2 pi y)))))

(defun-g min-vec4 ((invec4 :vec4) (min-value :float))
  (vec4 (min (aref invec4 0) min-value)
        (min (aref invec4 1) min-value)
        (min (aref invec4 2) min-value)
        1.0))

(defun-g max-vec4 ((invec4 :vec4) (min-value :float))
  (vec4 (max (aref invec4 0) min-value)
        (max (aref invec4 1) min-value)
        (max (aref invec4 2) min-value)
        1.0))

(defun-g id-to-uv-offset ((id :int) (atlas-size :float))
  (vec2 (/ (float (mod id atlas-size)) atlas-size)
        (/ (float (/ id atlas-size)) atlas-size)))

(defun-g atlas-column-row-to-uv-offset ((column :float) (row :float) (atlas-size :float))
  (vec2 (/ column atlas-size)
        (/ row atlas-size)))

(defun-g 1d-to-3d ((index :float) (cols :float) (depth :float))
  (let* ((z (int (/ index (* cols depth))))
         (index (- index (* z cols depth)))
         (x (mod index cols))
         (y (int (/ index cols))))
    (vec3 x y z)))

(defun-g 1d-to-2d ((index :float) (cols :float))
  (let* ((x (mod index cols))
         (y (int (/ index cols))))
    (vec2 x y)))

(defun-g calc-uv ((row :float) (col :float) (atlas-size :float) (uv :vec2))
  (let ((atlas-offset (/ 1.0 atlas-size)))
    (vec2 (+ (* row atlas-offset) (* (aref uv 0) atlas-offset))
          (+ (* col atlas-offset) (* (aref uv 1) atlas-offset)))))

(defun-g float-eq ((floata :float) (floatb :float))
  (let ((tolerance 0.1f0))
    (and (< floata (+ floatb tolerance))
         (< (- floatb tolerance) floata))))

(defun-g float-eq-or ((floata :float) (floatb :float) (subsequent :float) (alternative :float))
  (if (float-eq floata floatb) subsequent alternative))


(defun-g decode-vert-data1 ((data1 :float))
  (let* ((data1 (* data1 100))
         (pos (round (/ data1 10)))
         (uv (round (- data1 (* 10 pos)))))
    (vec2 pos
          uv)))

(defun-g decode-vert-data2 ((data2 :float))
  (let* ((face-float (round data2))
         (texture-atlas-index (* data2 100000))
         (texture-atlas-index (- texture-atlas-index (* 100000 face-float))))
    (vec2 face-float
          (round texture-atlas-index))))
