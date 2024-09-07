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

(defun setup-lparallel-kernel (&optional (worker-threads 1))
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

(defun-g vert-stage ((vert block-vert)
                     &uniform
                     (cam-pos :vec3)
                     (cam-rot :vec3)
                     (now :float)
                     (proj :mat4)
                     (offset :vec3)
                     (chunk-width :int)
                     (chunk-height :int)
                     (atlas-size :float)
                     (lod :float))
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
         (pos (* pos (+ 1.0 (* 1.0 lod))))
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
                 ;; (vec4 -110
                 ;;       -64 -330 1)
                 (vec4 cam-pos 1)
                 ;;(vec4 -20 -15 -100 1)
                 ))

         (atlas-coords (1d-to-2d texture-atlas-index 256))
         (uv (1d-to-2d uv 2.0))
         (uv (calc-uv (aref atlas-coords 0) (aref atlas-coords 1) atlas-size uv))
         (pos (* proj pos rot)))
    
    (values pos;;(* proj pos rot)
            uv
            face-light-float
            pos)))


(defun-g frag-stage ((uv :vec2) (face-light-float :float) (pos :vec4) &uniform (texture-atlas-ssbo ssbo-struct :ssbo) (skylight-colour :vec3) (sky-colour :vec4))
  ;; (let* (
  ;;        )
  ;;   my-data)
  (let* ((uv-index (int (mod (* 1 (2d-to-1d-g (int (* 48 (/ (aref uv 0) 1)))
                                              (int (* 48 (/ (aref uv 1) 1)))
                                              48))
                             2304)))
         (texture-sample (/ (aref (data texture-atlas-ssbo) uv-index) 255.0))
         ;;(texture-sample (texture atlas-sampler uv))
         (sunlight-mult (face-light-float-to-multiplier face-light-float))
         (fog-mult (min 1 (/ 1 (* (abs (aref pos 2)) 0.01)))
           )
         (fog-colour (* skylight-colour 0.8)))

    (setf texture-sample (* texture-sample (vec4 sunlight-mult sunlight-mult sunlight-mult 1.0)))
    (setf texture-sample (* texture-sample (vec4 skylight-colour 1.0)))
    ;;(vec4 texture-sample 1.0)
    ;;texture-sample
    (vec4
     (lerp (aref fog-colour 0) (aref texture-sample 0) fog-mult)
     (lerp (aref fog-colour 1) (aref texture-sample 1) fog-mult)
     (lerp (aref fog-colour 2) (aref texture-sample 2) fog-mult)
     1.0)
    ;; (vec4 (mod (aref pos 0) 1.0)
    ;;       (mod (aref pos 1) 1.0)
    ;;       (mod (aref pos 2) 1.0)
    ;;       (aref pos 3))
    ;; (vec4 (mod (aref texture-sample 0) 1.0)
    ;;       (mod (aref texture-sample 1) 1.0)
    ;;       (mod (aref texture-sample 2) 1.0)
    ;;       1.0)
    ;; (vec4 1.0 0.0 0.0 1.0)
    ;; texture-sample
    )
  
  )

(defpipeline-g chunk-pipeline ()
  (vert-stage block-vert)
  (frag-stage :vec2 :float :vec4))


(defun-g screen-plane-vert-stage ((vert g-pt))
  (values (vec4 (pos vert) 1.0)
          (pos vert)
          (tex vert)))

(defun-g screen-plane-frag-stage ((pos :vec3) (texture-coordinate :vec2) &uniform (tex-sampler :sampler-2d))
  (vec4 (mod (aref pos 0) 1.0)
        (mod (aref pos 1) 1.0)
        (mod (aref pos 2) 1.0)
        1.0)
  (texture tex-sampler texture-coordinate)
  )

(defpipeline-g screen-plane-pipeline ()
  (screen-plane-vert-stage g-pt)
  (screen-plane-frag-stage :vec3 :vec2))


(let* ((time-divisor (coerce (/ internal-time-units-per-second (/ 1.0 1.0)) 'double-float)))
  (declare (type double-float time-divisor))
  (defparameter *now* (get-internal-real-time))
  (defparameter *now-double* (coerce *now* 'double-float))
  (defparameter *delta* 1.0)
  (defparameter *delta-double* 1d0)
  (defun update-now ()
    (let* ((previous-time *now-double*)
           (times (multiple-value-list (org.shirakumo.precise-time:get-precise-time)))
           (slack (* 100000 (truncate (/ (first times) 100000))))
           (seconds (- (first times) slack))
           (double-time 0d0)
           (double-time (+ double-time (/ (second times) 10000000) seconds)))
      (setf *now-double* double-time)
      (setf *now* (coerce *now-double* 'single-float))
      (setf *delta-double* (- *now-double* previous-time))
      (setf *delta* (coerce *delta-double* 'single-float))
      
      (update-sky-colour)
      *now*)))

(let* ()
  (defparameter *now-input* (coerce (get-internal-real-time) 'double-float))
  (defparameter *input-delta* 1d0)
  (defun update-now-input ()
    (let* ((prior-time *now-input*)
           (times (multiple-value-list (org.shirakumo.precise-time:get-precise-time)))
           (slack (* 100000 (truncate (/ (first times) 100000))))
           (seconds (- (first times) slack))
           (double-time 0d0)
           (double-time (+ double-time (/ (second times) 10000000) seconds)))
      (setf *now-input* (coerce double-time 'double-float))
      (setf *input-delta* (- *now-input* prior-time))
      *now-input*)))

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

(defparameter render-thread-context nil)

(defmacro setf-if-nil (place value)
  `(unless ,place
     (setf ,place ,value)))

(defun load-texture-atlas-to-c-array ()
  (or  (ignore-errors (dirt:load-image-to-texture "texture-atlas.png"))
       (ignore-errors (dirt:load-image-to-texture "projects/vox/texture-atlas.png"))))

(defparameter texture-atlas-image-data nil)
(defparameter texture-atlas-c-array nil)
(defparameter texture-atlas-gpu-array nil)
(defparameter texture-atlas-ssbo nil)

(defun init-render-thread ()
  (unless render-thread-context
    (setf render-thread-context (make-context-shared-with-current-context)))
  (bt:make-thread (lambda ()
                    (with-cepl-context (ctx render-thread-context)
                      (try-free-objects texture-atlas-image-data texture-atlas-c-array texture-atlas-gpu-array
                                        texture-atlas-ssbo)
                      (setf texture-atlas-image-data nil
                            texture-atlas-c-array nil
                            texture-atlas-gpu-array nil
                            texture-atlas-ssbo nil)
                      ;;(make-chunks radius-x width *chunk-height* radius-z)
                      (setf (cepl:depth-test-function) #'<)
                      (gl:enable :depth-test)
                      (setq my-depth-func (cepl:depth-test-function))

                      (setf texture-atlas-image-data
                                   (list
                                    (list
                                     (loop for row in (pull-g (load-texture-atlas-to-c-array))
                                                                              append (mapcar #'v! row)))))
                      (setf texture-atlas-c-array (make-c-array texture-atlas-image-data
                                                                       :dimensions 1
                                                                       :element-type 'ssbo-struct))
                      (setf texture-atlas-gpu-array (make-gpu-array texture-atlas-c-array
                                                                           :element-type 'ssbo-struct))
                      (setf texture-atlas-ssbo (make-ssbo texture-atlas-gpu-array 'ssbo-struct))
                      
                      (loop (funcall fbo-render-loop-func))))
                  :name "rendering-thread"))


(defparameter screen-plane (list (list (vec3 -1.0 -1.0 0.0) (vec2 0.0 0.0))
                                 (list (vec3 1.0 -1.0 0.0) (vec2 1.0 0.0))
                                 (list (vec3 1.0 1.0 0.0) (vec2 1.0 1.0))
                                 (list (vec3 -1.0 1.0 0.0) (vec2 0.0 1.0))))

(defparameter screen-plane-indices (vector 0 1 2 0 2 3))

(defparameter screen-plane-vert-array nil)
(defparameter screen-plane-index-array nil)
(defparameter screen-plane-buffer-stream nil)
(defun init (&optional (width *chunk-width*) (radius-x 8) (radius-z radius-x))
  ;; (unless *transform-feedback-array*
  ;;   (setf *transform-feedback-array* (make-gpu-array nil :element-type :vec3 :dimensions 1000)))
  ;; (unless *transform-feedback-stream*
  ;;   (setf *transform-feedback-stream* (make-transform-feedback-stream *transform-feedback-array*)))
  (setf (surface-title (current-surface)) "vox")
  (sdl2:set-relative-mouse-mode t)
  (setup-projection-matrix)
  (try-free-objects screen-plane-index-array screen-plane-vert-array screen-plane-buffer-stream)
  (setf screen-plane-vert-array (make-gpu-array screen-plane :element-type 'g-pt)
        screen-plane-index-array (make-gpu-array screen-plane-indices :element-type :uint)
        screen-plane-buffer-stream (make-buffer-stream screen-plane-vert-array :index-array screen-plane-index-array))
  ;;(init-render-thread)
  ;;(make-chunks radius-x width *chunk-height* radius-z)
  )

;;(defparameter *input-delta* 1d0)
(defparameter *last-frame-time* 1d0)
(defparameter *fps* 1)
(defparameter *blending-params* (make-blending-params))

(defun toggle-vsync ()
  (setf (cepl.sdl2::vsync)
        (not (cepl.sdl2::vsync))))

(defparameter camera-current-pos (vec3 0.0 0.0 0.0))
(defparameter camera-current-rot (vec3 0.0 0.0 0.0))

(defparameter *projection-matrix* nil)
;;(defparameter *transform-feedback-gpu-array* nil)
;;(defparameter *transform-feedback-stream* nil)

(defparameter cube-1 (list (vec3 0.0 1.0 0.0) ;;0   FRONT
                           (vec3 0.0 0.0 0.0) ;;1
                           (vec3 1.0 0.0 0.0) ;;2
                           (vec3 1.0 1.0 0.0) ;;3

                           (vec3 0.0 1.0 1.0) ;;4   BACK
                           (vec3 1.0 1.0 1.0) ;;5
                           (vec3 1.0 0.0 1.0) ;;6
                           (vec3 0.0 0.0 1.0) ;;7

                           (vec3 0.0 1.0 0.0) ;;8   LEFT
                           (vec3 0.0 0.0 0.0) ;;9
                           (vec3 0.0 0.0 1.0) ;;1.00
                           (vec3 0.0 1.0 1.0) ;;1.0

                           (vec3 1.0 1.0 0.0) ;;1.02   RIGHT
                           (vec3 1.0 1.0 1.0) ;;1.03
                           (vec3 1.0 0.0 0.0) ;;1.04
                           (vec3 1.0 0.0 1.0) ;;1.05

                           (vec3 0.0 1.0 1.0) ;;1.06  TOP
                           (vec3 0.0 1.0 0.0) ;;1.07
                           (vec3 1.0 1.0 0.0) ;;1.08
                           (vec3 1.0 1.0 1.0) ;;1.09

                           (vec3 0.0 0.0 1.0) ;;20  BOTTOM
                           (vec3 1.0 0.0 0.0) ;;21.0
                           (vec3 0.0 0.0 0.0) ;;22
                           (vec3 1.0 0.0 1.0) ;;23
                           ))

(defun-g basic-vert-stage ((vert :vec3)
                     &uniform
                     (now :float)
                     (proj :mat4)
                     (rot :vec3))
  (let* ((pos (* (rtg-math.matrix4:rotation-from-euler rot) (vec4 vert 1)))
         (col (if (or (isinf (aref pos 0))
                      (isinf (aref pos 1))
                      (isinf (aref pos 2)))
                  (vec3 1.0 0.0 0.0)
                  (vec3 0.0 0.0 1.0)))
         (pos (+ pos (vec4 (* 2 (sin now)) (* 3 (cos now)) -5 0))))
    (values (* proj pos)
            (vec3 (aref (* proj pos) 0)
                  (aref (* proj pos) 1)
                  (aref (* proj pos) 2)))))

(defun-g basic-frag-stage ((col :vec3) &uniform (texture-atlas-ssbo ssbo-struct :ssbo))
  (let* ((col (vec4 (mod (aref col 0) 1.0)
                    (mod (aref col 1) 1.0)
                    (mod (aref col 2) 1.0)
                    1.0))
         (uv-index (int (mod (* 1 (2d-to-1d-g (int (* 48 (/ (aref col 0) 1)))
                                              (int (* 48 (/ (aref col 1) 1)))
                                              48))
                             2304)))
         (my-data (/ (aref (data texture-atlas-ssbo) uv-index) 255.0)))
    my-data))

(defpipeline-g basic-pipeline ()
  (basic-vert-stage :vec3)
  (basic-frag-stage :vec3))

(defun step-rendering ()
  (unless *rendering-paused?*
    (with-fbo-bound ((default-fbo (cepl-context)))
      (ignore-errors
       (setf (resolution (current-viewport))
             (get-cepl-context-surface-resolution)))
      (setup-projection-matrix)
      (setf (clear-color) sky-colour)
      (clear)
      (setf camera-current-pos (vox-cam:cam-pos *camera*)
            camera-current-rot (vox-cam:cam-rot *camera*))
      (render-chunks)
      (swap))))

(defun init-chunk-gen-thread ()
  (bt:make-thread (lambda () (loop (funcall chunk-gen-thread-func))) :name "chunk-gen-thread"))

(defparameter chunk-gen-thread-func (lambda ()
                                      (if queued-primordial-chunks
                                          (let ((offset (pop queued-primordial-chunks)))
                                            (make-chunk offset
                                                        (vox-world-sample:make-random-chunk-blocks2d offset lod)))
                                          (sleep 0.0001))))

(defparameter inner-loader-thread-func (lambda ()
                                         (when chunks-queued-to-be-freed?
                                           (maphash (lambda (offset entry)
                                                      (try-free (car entry)))
                                                    *chunks-at-offsets-table*)
                                           (clrhash *chunks-at-offsets-table*)
                                           (setf chunks-queued-to-be-freed? nil))

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
                                                                    :retain-arrays nil
                                                                    )))
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
                                                 
                                                 (try-free-objects vert-array index-array)
                                                 )))))

(defun get-cepl-context-surface-resolution ()
  (surface-resolution (current-surface (cepl-context))))

(defparameter *max-framerate* 100)
(defparameter limit-input-polling? t)

(defparameter prior-time 1)
(defparameter main-loop-fps 123)
(defparameter main-loop-delta 1.0)
(defparameter main-loop-fps-buffer (make-array 1000))
(defparameter main-loop-fps-buffer-index 0)

(defun update-main-loop-fps ()
  (setf main-loop-fps-buffer-index (mod (1+ main-loop-fps-buffer-index) 1000))
  (setf main-loop-delta (- (update-now-input) prior-time))
  (setf (aref main-loop-fps-buffer main-loop-fps-buffer-index) (truncate (/ 1d0 (max 0.00000001 main-loop-delta))))                                 
  (setf main-loop-fps (truncate (/ 1d0 (max 0.00000001 main-loop-delta))))
  (setf prior-time *now-input*))

(defparameter main-loop-func (lambda ()
                               (livesupport:continuable
                                 (step-host)
                                 (update-inputs)
                                 (livesupport:update-repl-link)
                                 (update-main-loop-fps)
                                 (when limit-input-polling?
                                   (sleep 0.0001)))))

(defparameter fbo-render-loop-func (lambda ()
                                     (livesupport:continuable
                                       (if *rendering-paused?*
                                           (sleep 0.01)
                                           (progn
                                             (funcall render-loop-func)
                                             (funcall inner-loader-thread-func))))))

(defparameter render-loop-func (lambda ()
                                 (let* ((target-delta (/ 1d0 *max-framerate*))
                                        (start-time (progn (update-now) *now-double*))
                                        (time-since-last-frame (- start-time *last-frame-time*)))
                                   (if (>= time-since-last-frame target-delta)
                                       (progn (step-rendering)
                                              (setf *fps* (truncate (/ 1.0d0 (- *now-double* *last-frame-time*))))
                                              (setf *last-frame-time* *now-double*))
                                       (sleep 0.0001)))))

(defun update-inputs ()
  (update-now-input)
  (vox-cam:update-camera *camera* *input-delta* *now-input*))

(defun main ()
  (ignore-errors (cepl:repl 720 480))
  (init)
  (setf (cepl.sdl2::vsync) nil)
  (with-paused-rendering
    (resolve-textures))
  (init-render-thread)
  (init-chunk-gen-thread)
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
    (setf chunks-queued-to-be-freed? t)
    (setf queued-primordial-chunks nil)))


