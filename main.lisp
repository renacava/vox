(in-package #:vox)

(defparameter *projection-matrix* nil)
(defparameter *my-chunk* nil)
(defparameter *my-chunk2* nil)
(defparameter *my-chunks* nil)
(defparameter *texture-atlas-tex* nil)
(defparameter *texture-atlas-sampler* nil)

(defun try-free (object)
  (when object (free object)))

(defun try-free-objects (&rest objects)
  (mapcar #'try-free objects))

(defstruct-g block-vert
  (pos :vec3)
  (uv :vec2))

(defun-g vert-stage ((block-vert block-vert)
                     &uniform
                     (now :float)
                     (proj :mat4)
                     (offset :vec3)
                     (chunk-width :int))
  (let* ((pos (vec4 (block-vert-pos block-vert) 1))
         (offset (* offset chunk-width))
         (pos (+ pos (vec4 offset 0)))
         (pos (+ pos (vec4 (- (* 70 (sin now)) 60) (- (* 15 (cos now)) 7) -20 0))))
    (values (* proj pos)
            (block-vert-uv block-vert))))

(defun-g frag-stage ((uv :vec2) &uniform (atlas-sampler :sampler-2d))
  (texture atlas-sampler uv))

(defpipeline-g basic-pipeline ()
  (vert-stage block-vert)
  (frag-stage :vec2))

(defun now ()
  (float (/ (get-internal-real-time) 1000)))

(defparameter *rendering-paused?* nil)

(defun make-chunks (n &optional (width 8))
  (mapcar #'try-free *my-chunks*)
  (setf *my-chunks* (loop for i below n
                          collect (make-chunk :width width :offset (list 0 0 (- i))))))

(defun make-chunks (radius &optional (width 8))
  (mapcar #'try-free *my-chunks*)
  (setf *my-chunks* (loop for i below radius
                          append (loop for j below radius
                                       collect (make-chunk :width width :offset (list i 0 (- j))))
                          )))

(defun init (&optional (width 16) (radius 8))
  (setf *rendering-paused?* t)
  (setf (surface-title (current-surface)) "vox")
  (try-free-objects *texture-atlas-tex* *texture-atlas-sampler*)
  (setf *texture-atlas-tex* (or 
                             (ignore-errors (dirt:load-image-to-texture "texture-atlas.png"))
                             (ignore-errors (dirt:load-image-to-texture "projects/vox/texture-atlas.png"))))
  (setf *texture-atlas-sampler* (sample *texture-atlas-tex*
                                        :minify-filter :nearest-mipmap-nearest
                                        :magnify-filter :nearest))
  
  ;; (setf *my-chunk* (make-chunk :width width :offset (list 0 0 0)))
  ;; (setf *my-chunk2* (make-chunk :width width :offset (list 0 0 -1)))
  (make-chunks radius width)
  (setf *projection-matrix* (rtg-math.projection:perspective (x (resolution (current-viewport)))
                                                             (y (resolution (current-viewport)))
                                                             0.1
                                                             300f0
                                                             60f0))
  (setf *rendering-paused?* nil))

(defun step-rendering ()
  (unless *rendering-paused?*
    (clear)
    (loop for chunk in *my-chunks*
          do (map-g #'basic-pipeline (buffer-stream chunk)
                    :now (now)
                    :proj *projection-matrix*
                    :offset (offset chunk)
                    :chunk-width (width chunk)
                    :atlas-sampler *texture-atlas-sampler*))
    ;; (when *my-chunk*
    ;;   (map-g #'basic-pipeline (buffer-stream *my-chunk*)
    ;;          :now (now)
    ;;          :proj *projection-matrix*
    ;;          :offset (offset *my-chunk*)
    ;;          :chunk-width (width *my-chunk*)
    ;;          :atlas-sampler *texture-atlas-sampler*)
    ;;   (map-g #'basic-pipeline (buffer-stream *my-chunk2*)
    ;;          :now (now)
    ;;          :proj *projection-matrix*
    ;;          :offset (offset *my-chunk2*)
    ;;          :chunk-width (width *my-chunk2*)
    ;;          :atlas-sampler *texture-atlas-sampler*))
    
    (step-host)
    (swap)))


(defparameter main-loop-func (lambda ()
                               (livesupport:continuable
                                 (step-rendering)
                                 (step-host)
                                 (livesupport:update-repl-link)
                                 (sleep 0.025))
                               

                               ))

(defun main ()
  (cepl:repl 720 480)
  (init)
  (loop (funcall main-loop-func)))


