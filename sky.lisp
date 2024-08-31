(in-package #:vox)

(defmacro without-depth (&body body)
  `(progn
     (gl:disable :depth-test)
     ,@body
     (gl:enable :depth-test)))

(defparameter *sky-colours* (list
                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ;; alway day
                             ;; (list 0 (vec3 0.3 0.55 1.0))
                             ;; (list 0.5 (vec3 0.3 0.3 1.0))
                             ;; (list 1 (vec3 0.3 0.55 1.0))

                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ;; actual daytime colours
                             (list (/ 0 24) (vec3 0.02 0.0 0.05)) ;; midnight
                             (list (/ 2 24) (vec3 0.03 0.03 0.1)) ;; night
                             (list (/ 4 24) (vec3 0.5 0.3 0.2)) ;; dawn
                             (list (/ 7 24) (vec3 0.3 0.55 1.0)) ;; morning
                             (list (/ 12 24) (vec3 0.3 0.55 1.0)) ;; noon
                             (list (/ 16 24) (vec3 0.3 0.3 1.0)) ;; afternoon
                             (list (/ 17 24) (vec3 0.7 0.5 0.4)) ;; dusk
                             (list (/ 19 24) (vec3 0.3 0.2 0.35)) ;; twilight
                             (list (/ 21 24) (vec3 0.03 0.03 0.1)) ;; night
                             (list (/ 24 24) (vec3 0.02 0.0 0.05)) ;; midnight
                             ))
(defparameter sky-colour (vec4 0.0 0.0 0.0 1.0))

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

(defun-g star-vert ((vert :vec3)
                    &uniform
                    (now :float)
                    (proj :mat4))
  (let* ((vert (vec4 vert 1.0)))
    (values vert vert)))

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
       star4)))

(defpipeline-g star-pipeline ()
  (star-vert :vec3)
  (star-frag :vec4))

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

(defparameter *seconds-per-day* 60)

(defun update-sky-colour (&optional (time *now*))
  (let ((sky-col (lerp-vec3s (change-range (sin (* time 3 (/ 1 *seconds-per-day*))) -1 1 0 1) *sky-colours*)))
    (setf sky-colour
          (vec4 (aref sky-col 0)
                (aref sky-col 1)
                (aref sky-col 2)
                1.0))))

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
      (setf sky-buffer
            (make-buffer-stream
             (make-gpu-array (first my-cool-rect))
             :index-array (make-gpu-array (second my-cool-rect)))))
    
    (without-depth
      (map-g #'star-pipeline sky-buffer
             :now *now*
             :sky-colour sky-colour))))


