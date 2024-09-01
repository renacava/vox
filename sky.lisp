(in-package #:vox)

(defmacro without-depth (&body body)
  `(progn
     (gl:disable :depth-test)
     ,@body
     (gl:enable :depth-test)))

(defparameter *always-day?* nil)
(defparameter *sky-colours*
  (lambda () (if *always-day?*
                 (list (list 0 (vec3 0.3 0.55 1.0))
                       (list 0.5 (vec3 0.3 0.3 1.0))
                       (list 1 (vec3 0.3 0.55 1.0)))
                 (list (list (/ 0 24) (vec3 0.02 0.0 0.05)) ;; midnight
                       (list (/ 2 24) (vec3 0.03 0.03 0.1)) ;; night
                       (list (/ 4 24) (vec3 0.5 0.3 0.2)) ;; dawn
                       (list (/ 7 24) (vec3 0.3 0.55 1.0)) ;; morning
                       (list (/ 12 24) (vec3 0.3 0.55 1.0)) ;; noon
                       (list (/ 16 24) (vec3 0.3 0.3 1.0)) ;; afternoon
                       (list (/ 17 24) (vec3 0.7 0.5 0.4)) ;; dusk
                       (list (/ 19 24) (vec3 0.3 0.2 0.35)) ;; twilight
                       (list (/ 21 24) (vec3 0.03 0.03 0.1)) ;; night
                       (list (/ 24 24) (vec3 0.02 0.0 0.05))) ;; midnight
                 )))
  
(defparameter sky-colour (vec4 0.0 0.0 0.0 1.0))

(defun-g star-vert ((vert :vec4)
                    &uniform
                    (now :float)
                    (cam-rot :vec3)
                    (cam-pos :vec3)
                    (proj :mat4))
  (let* ((vert (1d-to-3d (aref vert 0) 2 2))
         (vert (vec4 vert 1.0))
         (pre-pos vert)
         (vert (- vert (vec4 0.5 0.5 0.5 0.0)))
         (vert (* vert 100.0))
         ;;(vert (* vert 100.0))
         ;;(vert (* vert 100.0))
         (rot-mat (rtg-math.matrix4:rotation-from-euler cam-rot))
         ;;(vert (* vert 10000000.0))
         ;;(vert (- vert (vec4 0.5 0.5 0.5 0.4)))
         ;;(cam-pos (vec4 cam-pos 0.0))
         ;;(vert (+ vert cam-pos))
         ;;(vert (* vert 100.0))
         (vert (* proj vert rot-mat))
         ;;vert
         
         ;;(vert (* vert -10000))
         ;;(rot-mat (rtg-math.matrix4:rotation-from-euler cam-rot))
         ;;(vert (* rot-mat vert))
         )
    (values vert pre-pos)))

(defun-g star-frag ((pos :vec4) &uniform (now :float) (sky-colour :vec4) (cam-rot :vec3) (proj :mat4))
  (let* ((sky-brightness (max (aref sky-colour 0)
                              (aref sky-colour 1)
                              (aref sky-colour 2)))
         (pos (vec4 (+ (aref pos 0) (* 0.0001 now))
                    (+ (aref pos 1) (* 0.0001 now))
                    (aref pos 2)
                    (aref pos 3)))
         
         
         (perlin1 (nineveh:cellular-noise-fast (* 10 (vec2 (aref pos 0) (aref pos 1)))))
         (perlin1 (min 1.0 (max 0.0 (* perlin1 2))))
         (star1 (nineveh:stars-noise (* 10 (vec2 (aref pos 0) (aref pos 1)))
                                     0.9 0.0 20))
         (star1 (* (vec4 star1 star1 star1 1.0) perlin1))
         (star1 (vec4 (* (aref star1 0) (+ 0.9 (mod (aref pos 0) 0.1)))
                      (* (aref star1 1) (+ 0.9 (mod (aref pos 1) 0.1)))
                      (* (aref star1 2) (+ 0.9 (mod (aref pos 2) 0.1)))
                      1.0))
         (star1 (* star1 0.75))
         
         (star1 (* star1 (- 1 sky-brightness)))

         ;;;;;;;;;;;;;;;;;;;

         (perlin3 (nineveh:perlin-noise (* 1 (vec2 (aref pos 1) (aref pos 0)))))
         (perlin3 (min 1.0 (max 0.0 (* perlin3 1.0))))
         (star3 (nineveh:stars-noise (* 80 (vec2 (aref pos 0) (aref pos 1)))
                                     0.005 0.0 15))
         (star3 (* (vec4 star3 star3 star3 1.0) perlin3))
         (star3 (vec4 (* (aref star3 0) (+ 0.7 (mod (aref pos 0) 0.3)))
                      (* (aref star3 1) (+ 0.4 (mod (aref pos 1) 0.1)))
                      (* (aref star3 2) (+ 0.5 (mod (aref pos 2) 0.5)))
                      1.0))
         (star3 (* star3 3))
         (star3 (* star3 (- 1 sky-brightness)))

         ;;;;;;;;;;;;;;;;;;;
         
         (pos (* pos (vec4 2.0 2.0 1.0 1.0)))
         (perlin4 (nineveh:perlin-noise (* 10.5 (vec2 (aref pos 0) (sin (* 0.00001 now))))))
         (star4 (nineveh:stars-noise (* 100 (vec2 (aref pos 0) (aref pos 1)))
                                     1.0 0.0 16))
         (star4 (* (vec4 star4 star4 star4 1.0) perlin4))
         (star4 (vec4 (* (aref star4 0) (+ 0.8 (mod (aref pos 0) 0.2)))
                      (* (aref star4 1) (+ 0.8 (mod (aref pos 1) 0.2)))
                      (* (aref star4 2) (+ 0.8 (mod (aref pos 2) 0.2)))
                      1.0))
         (star4 (* star4 2))
         
         (star4 (* star4 (- 1 sky-brightness)))
         
         (star1 (max-vec4 (min-vec4 star1 1.0) 0.0))
         (star3 (max-vec4 (min-vec4 star3 1.0) 0.0))
         (star4 (max-vec4 (min-vec4 star4 1.0) 0.0)))
    
    
    (+ sky-colour
       star1
       ;;star3
       ;;star4
       )
    ;; (vec4 (mod (aref pos 0) 1.0)
    ;;       (mod (aref pos 1) 1.0)
    ;;       (mod (aref pos 2) 1.0)
    ;;       1.0)
    ;;(vec4 1.0 0.0 0.0 1.0)
    ))

(defpipeline-g star-pipeline ()
  (star-vert :vec4)
  (star-frag :vec4))

(defun lerp-vec3 (v1 v2 delta)
  (vec3 (lerp (aref v1 0) (aref v2 0) delta)
        (lerp (aref v1 1) (aref v2 1) delta)
        (lerp (aref v1 2) (aref v2 2) delta)))

(defun lerp-vec3-between-numbers (v1 v2 current-number lower-number higher-number)
  (lerp-vec3 v1 v2 (delta-between lower-number higher-number current-number)))

(defun get-bounds-from-numbers (number numbers)
  "Returns the two numbers adjacent to number in the given sorted list of numbers, and the indices to elt them from within the given numbers list."
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

(defparameter *seconds-per-day* 60)

(defun update-sky-colour (&optional (time *now*))
  (let* ((sky-col (lerp-vec3s (change-range (sin (* time 3 (/ 1 *seconds-per-day*)))
                                            -1 1 0 1)
                              (resolve *sky-colours*))))
    (setf sky-colour
          (vec4 (aref sky-col 0)
                (aref sky-col 1)
                (aref sky-col 2)
                1.0))))

(let (sky-buffer)
  (defparameter sky-box
    (let* ((cube-mesh (copy-list (build-cube-mesh-from-faces `(top bottom left right front back))))
           (vert-array (loop for vert in (first cube-mesh)
                             collect (vec4 (first vert) (second vert) (third vert) 1.0)))
           (index-array (copy-list ;;(second cube-mesh)
                                   (reverse (second cube-mesh))
                                   )))
      (list (make-c-array vert-array)
            (make-c-array index-array :element-type :uint))))

  
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
      (setf sky-buffer
            (make-buffer-stream
             (make-gpu-array (first sky-box))
             :index-array (make-gpu-array (second sky-box)))))
    
    (without-depth
      (map-g #'star-pipeline sky-buffer
             :now *now*
             :proj *projection-matrix*
             :sky-colour sky-colour
             :cam-rot (vox-cam:cam-rot *camera*)
             :cam-pos (vox-cam:cam-pos *camera*)))))


