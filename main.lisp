(in-package #:vox)

(defparameter *projection-matrix* nil)
(defparameter *rendering-paused?* nil)
(defparameter *camera* (vox-cam:make-camera))

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

(defstruct-g block-vert
  (data1 :float)
  (data2 :float))

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
  (let* ((local-offset-index (floor data1))
         (data1 (* data1 100))
         (data1 (- data1 (* local-offset-index 100)))
         (pos (round (/ data1 10)))
         (uv (round (- data1 (* 10 pos)))))
    (vec3 pos
          uv
          local-offset-index)))

(defun-g decode-vert-data2 ((data2 :float))
  (let* ((face-float (round data2))
         (texture-atlas-index (* data2 100000))
         (texture-atlas-index (- texture-atlas-index (* 100000 face-float))))
    (vec2 face-float
          (round texture-atlas-index))))

(defun-g vert-stage ((vert block-vert)
                     &uniform
                     (cam-pos :vec3)
                     (cam-rot :vec3)
                     (now :float)
                     (proj :mat4)
                     (offset :vec3)
                     (chunk-width :int)
                     (chunk-height :int)
                     (atlas-size :float))
  (let* ((data1 (block-vert-data1 vert))
         (data1 (decode-vert-data1 data1))
         (pos (aref data1 0))
         (uv (aref data1 1))
         (local-offset-index (aref data1 2))

         (rot (rtg-math.matrix4:rotation-from-euler cam-rot))

         (data2 (block-vert-data2 vert))
         (data2 (decode-vert-data2 data2))
         (face-light-float (aref data2 0))
         (texture-atlas-index (aref data2 1))

         
         
         (pos (1d-to-3d pos 2.0 2.0))
         (pos (+ pos (1d-to-3d local-offset-index chunk-width chunk-height)))
         (pos (vec4 pos 1))
         (offset (* offset chunk-width))
         (pos (+ pos (vec4 offset 0)))
         (pos (* pos 0.5))
         (now (* 1.5 now))
         (pos (+ pos
                 ;; (vec4
                 ;;  (+ -256 ;;(* -100 (sin (* 0.25 now)))
                 ;;     )
                 ;;  (+ -140 ;;(* 20 (sin now))
                 ;;     )
                 ;;  (+ -760 ;;(* 25 (+ 1 (sin (* 2.0 now))))
                 ;;     ))
                 ;; (vec4
                 ;;  (+ -100 (sin (* 2 now)))
                 ;;  -32
                 ;;  -15)
                 (vec4 -110
                       -64 -330 1)
                 (vec4 cam-pos 1)
                 ;;(vec4 -20 -15 -100 1)
                 ))

         (atlas-coords (1d-to-2d texture-atlas-index 256))
         (uv (1d-to-2d uv 2.0))
         (uv (calc-uv (aref atlas-coords 0) (aref atlas-coords 1) atlas-size uv)))
    
    (values (* proj pos rot)
            uv
            face-light-float
            pos)))


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
     1.0)))

(defpipeline-g basic-pipeline ()
  (vert-stage block-vert)
  (frag-stage :vec2 :float :vec4))

(let* ((time-divisor (coerce (/ internal-time-units-per-second (/ 1.0 1.0)) 'double-float))
       ;;(dtime-divisor (coerce (/ org.shirakumo.precise-time:precise-time-units-per-second )))
       )
  (declare (type double-float time-divisor))
  (defparameter *now* (get-internal-real-time))
  (defparameter *now-double* (coerce *now* 'double-float))
  (defun update-now ()
    (let* (
           (times (multiple-value-list (org.shirakumo.precise-time:get-precise-time)))
           (slack (* 100000 (truncate (/ (first times) 100000))))
           (seconds (- (first times) slack))
           (double-time 0d0)
           (double-time (+ double-time (/ (second times) 10000000) seconds)))
      (setf *now-double* double-time)
      ;;(setf *now-double* (coerce (/ (get-internal-real-time) time-divisor) 'double-float))
      (setf *now* (coerce *now-double* 'single-float))
      (update-sky-colour)
      *now*)
))

(defun setup-projection-matrix ()
  (setf *projection-matrix* (rtg-math.projection:perspective (x (resolution (current-viewport)))
                                                             (y (resolution (current-viewport)))
                                                             0.1
                                                             10000f0
                                                             90f0)))

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

(defparameter *transform-feedback-array* nil)
(defparameter *transform-feedback-stream* nil)

(defun init (&optional (width *chunk-width*) (radius-x 8) (radius-z radius-x))
  ;; (unless *transform-feedback-array*
  ;;   (setf *transform-feedback-array* (make-gpu-array nil :element-type :vec3 :dimensions 1000)))
  ;; (unless *transform-feedback-stream*
  ;;   (setf *transform-feedback-stream* (make-transform-feedback-stream *transform-feedback-array*)))
  (setf (surface-title (current-surface)) "vox")
  (sdl2:set-relative-mouse-mode t)
  (with-paused-rendering
    (resolve-textures))
  (setup-projection-matrix)
  (make-chunks radius-x width *chunk-height* radius-z))

(defparameter *delta* 1.0)
(defparameter *delta-double* (coerce *delta* 'double-float))
(defparameter *fps* 1)
(defparameter *blending-params* (make-blending-params))

(defun toggle-vsync ()
  (setf (cepl.sdl2::vsync)
        (not (cepl.sdl2::vsync))))

(defun step-rendering ()
  (unless *rendering-paused?*
    (setf (clear-color) sky-colour)
    (clear)
    (update-now)
    (vox-cam:update-camera *camera* *delta*)
    (setup-projection-matrix)
    (with-blending *blending-params*
      (render-night-sky)
      (render-chunks))
    (swap)))

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
                                                   (when existing-chunk (try-free (first existing-chunk)))
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
                                     (let ((start-time (progn (update-now) *now-double*)))
                                       (ignore-errors
                                        (setf (resolution (current-viewport))
                                              (get-cepl-context-surface-resolution)))
                                       (step-rendering)
                                       (step-host)
                                       (livesupport:update-repl-link)

                                       (update-now)

                                       (setf *delta-double* (- *now-double* start-time))
                                       (setq my-stuff (list *delta-double* *now-double* start-time))
                                       (setf *delta* (coerce *delta-double* 'single-float))
                                       (setf *fps* (truncate (/ 1.0d0 (if (= *delta-double* 0.0)
                                                                        0.00001d0
                                                                        *delta-double*))))))
                                 (funcall inner-loader-thread-func))))

(defun main ()
  (ignore-errors (cepl:repl 720 480))
  (init)
  (loop (funcall main-loop-func)))

(defun pause ()
  (if (setf *rendering-paused?* (not *rendering-paused?*))
      'paused
      'unpaused))

(defun destroy-world ()
  (with-paused-rendering
    (ignore-errors (lparallel:kill-tasks :default))
    (ignore-errors (lparallel:end-kernel))
    (setup-lparallel-kernel)
    (setf chunks-queued-to-be-freed? t)))


