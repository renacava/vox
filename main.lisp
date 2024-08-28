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

(defstruct-g block-vert
  (vert :float)
  (uv :float)
  (sunlit-mult :float)
  (texture-atlas-index :float)
  (local-offset :float)
  (sunlit-p :int))

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

(defun-g vert-stage ((vert block-vert)
                     &uniform
                     (now :float)
                     (proj :mat4)
                     (offset :vec3)
                     (chunk-width :int)
                     (chunk-height :int)
                     (atlas-size :float))
  (let* ((pos (1d-to-3d (block-vert-vert vert) chunk-width chunk-height))
         (pos (+ pos (1d-to-3d (block-vert-local-offset vert) chunk-width chunk-height)))
         (pos (vec4 pos 1))
         (offset (* offset chunk-width))
         (pos (+ pos (vec4 offset 0)))
         (pos (* pos 0.5))
         (now (* 1.5 now))
         (pos (+ pos (vec4 (+ -256 ;;(* -256 (sin (* 0.25 now)))
                              )
                           (+ -50 (sin now))
                           (+ -420 (* 200 (+ 1 (sin (* 1.5 now))))))))

         (atlas-coords (1d-to-2d (block-vert-texture-atlas-index vert) atlas-size))
         (uv (1d-to-2d (block-vert-uv vert) atlas-size))
         (uv (calc-uv (aref atlas-coords 0) (aref atlas-coords 1) atlas-size uv)))
    (values (* proj pos)
            uv
            (:flat (block-vert-sunlit-p vert))
            (block-vert-sunlit-mult vert)
            pos)))



(defparameter sky-colour (vec4 0.0 0.0 0.0 1.0))

(progn
  (setf sky-colour (vec4 0.0 0.45 1.0 1.0))
  (defun-g frag-stage ((uv :vec2) (sunlit-p :int) (sunlit-mult :float) (pos :vec4) &uniform (atlas-sampler :sampler-2d))
    (let* ((texture-sample (texture atlas-sampler uv))
           (is-sunlit (> sunlit-p 0))
           (fog-mult (min 1 (/ 1 (* (aref pos 2) -0.01))))
           (fog-colour (vec3 0.7 0.7 1.0)))
      (setf texture-sample (* texture-sample (vec4 sunlit-mult sunlit-mult sunlit-mult 1.0)))
      (unless is-sunlit
        (setf texture-sample (* texture-sample 0.9)))
      (vec4
       (lerp (aref fog-colour 0) (aref texture-sample 0) fog-mult)
       (lerp (aref fog-colour 1) (aref texture-sample 1) fog-mult)
       (lerp (aref fog-colour 2) (aref texture-sample 2) fog-mult)
       1.0))))












;; (progn
  
;;   ;;(setf sky-colour (vec4 0.0 0.45 1.0 1.0))
;;   (setf sky-colour (vec4 0.0 0.0 0.0 1.0))
;;   (defun-g frag-stage ((uv :vec2) (sunlit-p :int) (sunlit-mult :float) (pos :vec4) &uniform (atlas-sampler :sampler-2d))
;;    (let* ((sunlight-mult (if (> sunlit-p 0)
;;                              1.0
;;                              0.5))
;;           (texture-sample (texture atlas-sampler uv))
;;           ;;(sunlit-texture (* texture-sample sunlight-mult))
;;           ;;(height-lit-texture (* sunlit-texture (min (max 0 (- -0.2 (/ 24 (aref pos 1)))) 1)))
;;           ;; (depth-fogged-texture texture-sample;;height-lit-texture
;;           ;;   )
;;           (texture-sample (* texture-sample 0.7))
;;           ;;(texture-sample (* texture-sample (vec4 sunlit-mult sunlit-mult sunlit-mult 1.0)))
;;           (fog-mult (min 1 (/ 1 (* (aref pos 2) -0.01))))
;;           ;;(vis-mult (min 1 (/ 1 (* (aref pos 2) -0.005))))
;;           )
;;      ;; (setf (aref depth-fogged-texture 3)
;;      ;;       0.5)
;;      ;; (setf (aref depth-fogged-texture 2) (lerp (aref depth-fogged-texture 3)
;;      ;;                                           0
;;      ;;                                           0.5))
;;      ;;height-lit-texture
;;      ;;(vec4 0.35 0.35 1.0 1.0)
;;      (vec4
;;       (lerp 0.7 (aref texture-sample 0) fog-mult)
;;       (lerp 0.7 (aref texture-sample 1) fog-mult)
;;       (lerp 1.0 (aref texture-sample 2) fog-mult)
;;       ;;(lerp 0.0 (aref depth-fogged-texture 3) vis-mult)
;;       1.0
;;       ;;0.0
;;       )
;;      ;;depth-fogged-texture
;;      texture-sample
;;      )))

(defpipeline-g basic-pipeline ()
  (vert-stage block-vert)
  (frag-stage :vec2 :int :float :vec4))

(let ((time-divisor (coerce (/ internal-time-units-per-second (/ 1.0 6.0)) 'single-float)))
  (declare (type single-float time-divisor))
  (defparameter *now* (get-internal-real-time))
  (defun update-now ()
    (setf *now* (/ (get-internal-real-time) time-divisor))))

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
  (setf (clear-color) sky-colour)
  (setup-projection-matrix)
  ;;(load-texture-atlas)
  (make-chunks radius-x width *chunk-height* radius-z))

(defparameter *delta* 1.0)
(defparameter *fps* 1)

(defparameter my-buffers nil)

(defun step-rendering ()
  (unless *rendering-paused?*
    (clear)
    (update-now)
    (setup-projection-matrix)
    (maphash (lambda (offset entry)
               (render (car entry)))
             *chunks-at-offsets-table*)
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
