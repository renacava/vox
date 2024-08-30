(in-package #:vox)

(defparameter *projection-matrix* nil)
(defparameter *rendering-paused?* nil)

(defmacro with-paused-rendering (&body body)
  `(let ((prior-state *rendering-paused?*))
     (setf *rendering-paused?* t)
     (unwind-protect
          (progn
            ,@body)
       (setf *rendering-paused?* prior-state))))

(defun setup-lparallel-kernel (&optional (worker-threads 4))
  (unless lparallel:*kernel*
    (setf lparallel:*kernel* (lparallel:make-kernel worker-threads))))

(defun try-free (object)
  (when object (free object)
        (return-from try-free nil))
  object)

(defun try-free-objects (&rest objects)
  (mapcar #'try-free objects))

;; (defstruct-g block-vert
;;   (vert :float)
;;   (uv :float)
;;   (face-light-float :float)
;;   (texture-atlas-index :float)
;;   (local-offset :float))

(defstruct-g block-vert
  (vert-data :float)
  (texture-atlas-index :float)
  (local-offset :float))

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

(defun-g face-light-float-to-multiplier ((face-light-float :float))
  (labels ((fleq ((x :int) (subsequent :float) (alternative :float))
             (float-eq-or face-light-float x subsequent alternative)))

    (*
    (fleq 0   0.53   ;; TOP     UN-SUNLIT
    (fleq 1   0.5   ;; LEFT    UN-SUNLIT
    (fleq 2   0.5   ;; RIGHT   UN-SUNLIT
    (fleq 3   0.4   ;; FRONT   UN-SUNLIT
    (fleq 4   0.4   ;; BACK    UN-SUNLIT
    (fleq 5   0.32   ;; BOTTOM  UN-SUNLIT 

    (fleq 6   0.8   ;; TOP     SUNLIT
    (fleq 7   0.6   ;; LEFT    SUNLIT
    (fleq 8   0.6   ;; RIGHT   SUNLIT
    (fleq 9   0.5   ;; FRONT   SUNLIT
    (fleq 10  0.5   ;; BACK    SUNLIT
    (fleq 11  0.4   ;; BOTTOM  SUNLIT

    (fleq 12  1.0   ;; TOP     SUNLIT
    (fleq 13  0.7   ;; LEFT    SUNLIT
    (fleq 14  0.7   ;; RIGHT   SUNLIT
    (fleq 15  0.6   ;; FRONT   SUNLIT
    (fleq 16  0.6   ;; BACK    SUNLIT
    (fleq 17  0.5   ;; BOTTOM  SUNLIT

    (fleq 18  1.0   ;; TOP     SUNLIT
    (fleq 19  0.85   ;; LEFT    SUNLIT
    (fleq 20  0.85   ;; RIGHT   SUNLIT
    (fleq 21  0.75   ;; FRONT   SUNLIT
    (fleq 22  0.75   ;; BACK    SUNLIT
    (fleq 23  0.6   ;; BOTTOM  SUNLIT
          
              0.0)  ;; DEFAULT 
          
          )))))))))))))))))))))))
    (if (> face-light-float 5)
        1.0
        1.1))))

;; (defun-g decode-vert-data ((vert-data :float))
;;   (let* ((vert (- vert-data 1000000000))
;;          (pos (/ vert 100000000))
;;          (vert (- vert (* pos 100000000)))
;;          (uv (/ vert 10000000))
;;          (vert (- vert (* uv 10000000)))
;;          (face-float (/ vert 100000))
;;          (vert (- vert (* face-float 100000)))
;;          (texture-atlas-index vert))
;;     (vec4 (float pos)
;;           (float uv)
;;           (float face-float)
;;           (float texture-atlas-index))))

(defun-g decode-vert-data ((vert-data :float))
  (let* ((vert-data (round vert-data))
         (vert (- vert-data 10000))
         (pos (round (/ vert 1000)))
         (vert (- vert (* pos 1000)))
         (uv (round (/ vert 100)))
         (vert (- vert (* uv 100)))
         (face-float (float vert))
         )
    (vec3 pos
          uv
          face-float
          ;;:texture-atlas-index texture-atlas-index
          )))

(defun-g my-cool-perlin-noise ((x :float))
  (nineveh:perlin-noise (vec2 x x)))
  
(defun-g muller ((x :float) (y :float))
  (*
   (sqrt (* -2 (log x)))
   (cos (float (* 2 pi y)))))

(defun-g vert-stage ((vert block-vert)
                     &uniform
                     (now :float)
                     (proj :mat4)
                     (offset :vec3)
                     (chunk-width :int)
                     (chunk-height :int)
                     (atlas-size :float))
  (let* ((vert-data (block-vert-vert-data vert))
         (vert-data (decode-vert-data vert-data))
         (pos (aref vert-data 0))
         (uv (aref vert-data 1))
         (face-light-float (aref vert-data 2))
         (texture-atlas-index (block-vert-texture-atlas-index vert))
         
         (pos (1d-to-3d pos 2.0 2.0))
         (pos (+ pos (1d-to-3d (block-vert-local-offset vert) chunk-width chunk-height)))
         (pos (vec4 pos 1))
         (offset (* offset chunk-width))
         (pos (+ pos (vec4 offset 0)))
         (pos (* pos 0.5))
         (now (* 1.5 now))
         (pos (+ pos
                 (vec4 (+ -256 (* -100 (sin (* 0.25 now))))
                       (+ -34 ;;(* 20 (sin now))
                          )
                       (+ -520 ;;(* 25 (+ 1 (sin (* 2.0 now))))
                          ))
                 ;; (vec4
                 ;;  (+ -100 (sin (* 2 now)))
                 ;;  -32
                 ;;  -15)
                 ;;(vec4 -36 -32 -100 1)
                 ))

         (atlas-coords (1d-to-2d texture-atlas-index 256))
         (uv (1d-to-2d uv 2.0))
         (uv (calc-uv (aref atlas-coords 0) (aref atlas-coords 1) atlas-size uv)))
    (values (* proj pos)
            uv
            ;;(1d-to-2d face-light-float)
            face-light-float
            pos)))





(defparameter *sky-colours* (list
                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ;; alway day
                             (list 0 (vec3 0.3 0.55 1.0))
                             (list 0.5 (vec3 0.3 0.3 1.0))
                             (list 1 (vec3 0.3 0.55 1.0))

                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ;; actual daytime colours
                             ;;(list (/ 0 24) (vec3 0.02 0.0 0.05)) ;; midnight
                             ;;(list (/ 2 24) (vec3 0.03 0.03 0.1)) ;; night
                             ;;(list (/ 4 24) (vec3 0.5 0.3 0.2)) ;; dawn
                             ;;(list (/ 7 24) (vec3 0.3 0.55 1.0)) ;; morning
                             ;;(list (/ 12 24) (vec3 0.3 0.55 1.0)) ;; noon
                             ;;(list (/ 16 24) (vec3 0.3 0.3 1.0)) ;; afternoon
                             ;;(list (/ 17 24) (vec3 0.7 0.5 0.4)) ;; dusk
                             ;;(list (/ 19 24) (vec3 0.3 0.2 0.35)) ;; twilight
                             ;;(list (/ 21 24) (vec3 0.03 0.03 0.1)) ;; night
                             ;;(list (/ 24 24) (vec3 0.02 0.0 0.05)) ;; midnight
                             ))
(defparameter sky-colour (vec4 0.0 0.0 0.0 1.0))

(progn
  (setf sky-colour (vec4 0.0 0.45 1.0 1.0))
  (defun-g frag-stage ((uv :vec2) (face-light-float :float) (pos :vec4) &uniform (atlas-sampler :sampler-2d) (skylight-colour :vec3) (sky-colour :vec4))
    (let* ((texture-sample (texture atlas-sampler uv))
           (sunlight-mult (face-light-float-to-multiplier face-light-float))
           (fog-mult (min 1 (/ 1 (* (aref pos 2) -0.01))))
           (fog-colour (* skylight-colour 0.8)))

      (setf texture-sample (* texture-sample (vec4 sunlight-mult sunlight-mult sunlight-mult 1.0)))

      (setf texture-sample (* texture-sample (vec4 skylight-colour 1.0)))
      
      (vec4
       (lerp (aref fog-colour 0) (aref texture-sample 0) fog-mult)
       (lerp (aref fog-colour 1) (aref texture-sample 1) fog-mult)
       (lerp (aref fog-colour 2) (aref texture-sample 2) fog-mult)
       1.0)
      
      )))

(defun-g star-vert ((vert :vec3)
                    &uniform
                    (now :float)
                    (proj :mat4))
  (let* ((vert (vec4 vert 1.0)))
    (values vert vert)))

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

(defun-g star-frag ((pos :vec4) &uniform (now :float) (sky-colour :vec4))
  (let* ((sky-brightness (max (aref sky-colour 0)
                              (aref sky-colour 1)
                              (aref sky-colour 2)))
         (pos (vec4 (+ (aref pos 0) (* 0.00075 now)
                       )
                    (+ (aref pos 1) (* 0.00075 now)
                       )
                    (aref pos 2)
                    (aref pos 3)))
         (perlin1 (nineveh:cellular-noise-fast (* 10 (vec2 (aref pos 0) (aref pos 1) ;;(aref pos 2)
                                                          ))))
         (perlin1 (min 1.0 (max 0.0 (* perlin1 2))))
         (star1 (nineveh:stars-noise (* 100 (vec2 (aref pos 0) (aref pos 1)))
                                     0.9 0.0 20))
         (star1 (* (vec4 star1 star1 star1 1.0) perlin1))
         (star1 (vec4 (* (aref star1 0) (+ 0.9 (mod (aref pos 0) 0.1)))
                      (* (aref star1 1) (+ 0.9 (mod (aref pos 1) 0.1)))
                      (* (aref star1 2) (+ 0.9 (mod (aref pos 2) 0.1)))
                      1.0))
         (star1 (* star1 1.0))
         
         (star1 (* star1 (- 1 sky-brightness)))


         
         ;; (perlin2 (nineveh:perlin-noise (* 3 (vec2 (aref pos 1) (aref pos 0) ;;(aref pos 2)
         ;;                                                   ))))
         ;; (perlin2 (min 1.0 (max 0.0 (* perlin2 1.0))))
         ;; (star2 (nineveh:stars-noise (* 30 (vec2 (aref pos 0) (aref pos 1)))
         ;;                             0.3 0.0 30))
         ;; (star2 (* (vec4 star2 star2 star2 1.0) perlin2))
         ;; (star2 (vec4 (* (aref star2 0) (+ 0.7 (mod (aref pos 0) 0.3)))
         ;;              (* (aref star2 1) (+ 0.7 (mod (aref pos 1) 0.3)))
         ;;              (* (aref star2 2) (+ 0.7 (mod (aref pos 2) 0.3)))
         ;;              1.0))
         ;; (star2 (* star2 30))
         
         ;; (star2 (* star2 (- 1 sky-brightness)))



         (perlin3 (nineveh:perlin-noise (* 1 (vec2 (aref pos 1) (aref pos 0) ;;(aref pos 2)
                                                   ))))
         (perlin3 (min 1.0 (max 0.0 (* perlin3 1.0))))
         (star3 (nineveh:stars-noise (* 80 (vec2 (aref pos 0) (aref pos 1)))
                                     0.005 0.0 15))
         (star3 (* (vec4 star3 star3 star3 1.0) perlin3))
         (star3 (vec4 (* (aref star3 0) (+ 0.7 (mod (aref pos 0) 0.3)))
                      (* (aref star3 1) (+ 0.4 (mod (aref pos 1) 0.1)))
                      (* (aref star3 2) (+ 0.5 (mod (aref pos 2) 0.5)))
                      1.0))
         (star3 (* star3 2))
         
         (star3 (* star3 (- 1 sky-brightness)))


         (perlin4 (nineveh:perlin-noise (* 10.5 (vec2 (aref pos 0) (sin (* 0.00001 now))))))
         ;;(perlin4 (min 1.0 (max 0.0 (* perlin4 1.0))))
         (star4 (nineveh:stars-noise (* 100 (vec2 (aref pos 0) (aref pos 1)))
                                     1.0 0.0 16))
         (star4 (* (vec4 star4 star4 star4 1.0) perlin4))
         (star4 (vec4 (* (aref star4 0) (+ 0.8 (mod (aref pos 0) 0.2)))
                      (* (aref star4 1) (+ 0.8 (mod (aref pos 1) 0.2)))
                      (* (aref star4 2) (+ 0.8 (mod (aref pos 2) 0.2)))
                      1.0))
         (star4 (* star4 1))
         
         (star4 (* star4 (- 1 sky-brightness)))
         
         (star1 (max-vec4 (min-vec4 star1 1.0) 0.0))
         (star3 (max-vec4 (min-vec4 star3 1.0) 0.0))
         (star4 (max-vec4 (min-vec4 star4 1.0) 0.0)))
    
    
    (+ sky-colour
       star1
       star3
       star4
       )
    ))

(defpipeline-g star-pipeline ()
  (star-vert :vec3)
  (star-frag :vec4))



(let (sky-buffer)
  (defparameter my-cool-rect (list
                              (make-c-array
                               (list (vec4 -1.0 -1.0 0.0 1.0)
                                     (vec4 1.0 -1.0 0.0 1.0)
                                     (vec4 1.0 1.0 0.0 1.0)
                                     (vec4 -1.0 1.0 0.0 1.0)))
                              (make-c-array
                               (list 0 1 2 2 3 0) :element-type :uint)))

  (defun render-night-sky ()
    (unless sky-buffer
      (setf sky-buffer (make-buffer-stream (make-gpu-array (first my-cool-rect)) :index-array (make-gpu-array (second my-cool-rect)))))
    (map-g #'star-pipeline sky-buffer
           :now *now*
           :sky-colour sky-colour)))

(defun lerp-vec3 (v1 v2 delta)
  (vec3 (lerp (aref v1 0) (aref v2 0) delta)
        (lerp (aref v1 1) (aref v2 1) delta)
        (lerp (aref v1 2) (aref v2 2) delta)))

(defun lerp-vec3-between-numbers (v1 v2 current-number lower-number higher-number)
  (lerp-vec3 v1 v2 (delta-between lower-number higher-number current-number)))

(defun get-bounds-from-numbers (number numbers)
  "Returns the two numbers adjacent to number in the given sorted list of numbers."
  (loop for number-index below (1- (length numbers))
        do (let ((lower (elt numbers number-index))
                 (higher (elt numbers (1+ number-index))))
             (when (<= lower number higher)
               (return-from get-bounds-from-numbers
                 (list lower higher number-index (1+ number-index)))))))

(defun delta-between-numbers (number numbers)
  (let ((bounds (get-bounds-from-numbers number numbers)))
    (list (delta-between (first bounds) (second bounds) number)
          (third bounds)
          (fourth bounds))))

(defun lerp-vec3s (number number-vec3-pairs)
  (let ((delta-and-indices (delta-between-numbers number (mapcar #'first number-vec3-pairs))))
    (lerp-vec3 (second (elt number-vec3-pairs (second delta-and-indices)))
               (second (elt number-vec3-pairs (third delta-and-indices)))
               (first delta-and-indices))))

(defun delta-between (lower-bound upper-bound number)
  (float (change-range number lower-bound upper-bound 0 1)))

(defpipeline-g basic-pipeline ()
  (vert-stage block-vert)
  (frag-stage :vec2 :float :vec4))

(let ((time-divisor (coerce (/ internal-time-units-per-second (/ 1.0 6.0)) 'single-float)))
  (declare (type single-float time-divisor))
  (defparameter *now* (get-internal-real-time))
  (defun update-now ()
    (setf *now* (/ (get-internal-real-time) time-divisor))
    (let ((sky-col (lerp-vec3s (change-range (sin (* 0.2 *now*)) -1 1 0 1) *sky-colours*)))
      (setf sky-colour
            (vec4 (aref sky-col 0)
                  (aref sky-col 1)
                  (aref sky-col 2)
                  1.0)))


    
    *now*
    ))

(defun setup-projection-matrix ()
  (setf *projection-matrix* (rtg-math.projection:perspective (x (resolution (current-viewport)))
                                                             (y (resolution (current-viewport)))
                                                             0.1
                                                             10000f0
                                                             60f0)))

(defun load-texture-atlas ()
  (with-paused-rendering
    (ignore-errors
    (try-free-objects *texture-atlas-tex* *texture-atlas-sampler*)))
  (setf *texture-atlas-tex* (or 
                             (ignore-errors (dirt:load-image-to-texture "texture-atlas.png"))
                             (ignore-errors (dirt:load-image-to-texture "projects/vox/texture-atlas.png"))))
  (setf *texture-atlas-sampler* (sample *texture-atlas-tex*
                                        :minify-filter :linear-mipmap-linear
                                        :magnify-filter :nearest))
  (setf *texture-atlas-size*
        (coerce
         (truncate (/ (first (texture-base-dimensions *texture-atlas-tex*))
                      *texture-cell-size*))
         'single-float)))

(defun init (&optional (width *chunk-width*) (radius-x 8) (radius-z radius-x))
  (setf (surface-title (current-surface)) "vox")
  (with-paused-rendering
    (resolve-textures))
  (setup-projection-matrix)
  ;;(load-texture-atlas)
  (make-chunks radius-x width *chunk-height* radius-z))

(defparameter *delta* 1.0)
(defparameter *fps* 1)

(defparameter my-buffers nil)

(defun step-rendering ()
  (unless *rendering-paused?*
    (setf (clear-color) sky-colour)
    (clear)
    (update-now)
    (setup-projection-matrix)
    (with-blending (make-blending-params)
      (gl:disable :depth-test)
      (render-night-sky)
      (gl:enable :depth-test)
      (maphash (lambda (offset entry)
                 (render (car entry)))
               *chunks-at-offsets-table*)
      ;;(clear)
      )
    
    (step-host)
    (swap)))

(defparameter ctx nil)
(defparameter dirty? nil)
(defparameter flipflop nil)
(defparameter queued-chunks nil)
(defparameter chunk-queue-max-size 16)
(defparameter half-baked-chunks nil)
(defparameter chunks-queued-to-be-freed? nil)
(defparameter *chunks-at-offsets-table* (make-hash-table :test #'equal))

(defun queue-full? ()
  (< chunk-queue-max-size (length queued-chunks)))

(defun queue-chunk (mesh-data offset width height)
  (push (list mesh-data offset width height) queued-chunks))

(defparameter inner-loader-thread-func (lambda ()
                                         (if chunks-queued-to-be-freed?
                                             (with-paused-rendering
                                               (maphash (lambda (offset entry)
                                                          (try-free (car entry)))
                                                        *chunks-at-offsets-table*)
                                               (clrhash *chunks-at-offsets-table*)
                                               (setf chunks-queued-to-be-freed? nil)))
                                         
                                         (when queued-chunks
                                           (let* ((queued-chunk (pop queued-chunks))
                                                  (mesh-data (first queued-chunk))
                                                  (offset (v! (second queued-chunk)))
                                                  (width (third queued-chunk))
                                                  (height (fourth queued-chunk))
                                                  (vert-array (ignore-errors (make-gpu-array (first mesh-data))))
                                                  (index-array (ignore-errors (make-gpu-array (second mesh-data) :element-type :uint)))
                                                  (buffer-stream (when (and vert-array index-array)
                                                                   (make-buffer-stream
                                                                    vert-array
                                                                    :index-array index-array
                                                                    :retain-arrays nil)))
                                                  (chunk (when buffer-stream
                                                           (make-instance 'chunk
                                                                          :width width
                                                                          :height height
                                                                          :offset offset
                                                                          :vert-array vert-array
                                                                          :index-array index-array
                                                                          :buffer-stream buffer-stream))))
                                             (try-free-objects (first mesh-data) (second mesh-data))
                                             (if chunk
                                                 (let* ((offset (second queued-chunk))
                                                        (existing-chunk (gethash offset *chunks-at-offsets-table*)))
                                                   (when existing-chunk (try-free existing-chunk))
                                                   (setf (gethash offset *chunks-at-offsets-table*) (list chunk buffer-stream)))
                                                 (try-free-objects vert-array index-array))))))

(defun get-cepl-context-surface-resolution ()
  (surface-resolution (current-surface (cepl-context))))

(defparameter main-loop-func (lambda ()
                               (livesupport:continuable
                                 (if *rendering-paused?*
                                     (progn
                                       (step-host)
                                       (livesupport:update-repl-link)
                                       (sleep 0.01))
                                     (let ((start-time (update-now)))
                                       (ignore-errors
                                        (setf (resolution (current-viewport))
                                              (get-cepl-context-surface-resolution)))
                                       (step-rendering)
                                       (step-host)
                                       (livesupport:update-repl-link)
                                       (setf *delta* (- (update-now) start-time))
                                       (setf *fps* (truncate (/ 1.0 (if (= *delta* 0)
                                                                        1.0
                                                                        *delta*))))))
                                 (funcall inner-loader-thread-func))))

(defun main ()
  (ignore-errors (cepl:repl 720 480))
  (init)
  (loop (funcall main-loop-func))
  )

(defun pause ()
  (if (setf *rendering-paused?* (not *rendering-paused?*))
      'paused
      'unpaused))

(defun destroy-world ()
  (ignore-errors (lparallel:kill-tasks :default))
  (ignore-errors (lparallel:end-kernel))
  (setup-lparallel-kernel)
  (setf chunks-queued-to-be-freed? t))
