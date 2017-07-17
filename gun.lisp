(in-package #:org.shirakumo.fraf.shootman)

(define-asset (shootman bullet) texture
    (#p"bullet.png")
  :mag-filter :nearest)

(define-asset (shootman gun) texture
    (#p"gun.png")
  :mag-filter :nearest)

(defgeneric shoot (thing &key &allow-other-keys))

(define-shader-subject gun (base-entity pivoted-entity)
  ()
  (:default-initargs
   :pivot (vec 13 3 0)
   :size (vec 16 16)
   :texture (asset 'shootman 'gun)
   :vertex-array (asset 'shootman '16x)))

(defmethod shoot ((gun gun) &key from direction affinity))

(define-shader-subject basic-gun (gun)
  ((bullet-type :initarg :bullet-type :accessor bullet-type)
   (bullet-count :initarg :bullet-count :accessor bullet-count)
   (bullet-velocity :initarg :bullet-velocity :accessor bullet-velocity)
   (bullet-spread :initarg :bullet-spread :accessor bullet-spread)
   (bullet-arc :initarg :bullet-arc :accessor bullet-arc)
   (sound :initarg :sound :accessor sound))
  (:default-initargs
   :bullet-type 'bullet
   :bullet-count 1
   :bullet-velocity 4
   :bullet-spread 0
   :bullet-arc 0
   :sound (pool-path 'shootman #p"shot.mp3")))

(defmethod shoot ((gun basic-gun) &key from direction affinity)
  (harmony-simple:play (sound gun) :sfx
                       :type 'harmony-mp3:mp3-buffer-source
                       :location (list (vx from) (vy from) (vz from)))
  (loop repeat (bullet-count gun)
        for phi = (- (/ (bullet-arc gun) 2))
        then (+ phi (/ (bullet-arc gun) (bullet-count gun)))
        for spread = (if (= 0 (bullet-spread gun))
                         0
                         (- (random (bullet-spread gun)) (/ (bullet-spread gun) 2)))
        do (let ((dir (vrot (vxy_ direction) +vz+ (deg->rad (+ phi spread)))))
             (unless (v= 0 dir) (nvunit dir))
             (let* ((vel (nv* dir (bullet-velocity gun)))
                    (angle (acos (vx (vunit vel)))))
               (when (< (vy vel) 0) (setf angle (- angle)))
               (enter (load (make-instance (bullet-type gun)
                                           :angle angle
                                           :location (v+ from (v* dir 5))
                                           :velocity vel
                                           :affinity affinity))
                      *loop*)))))

(define-shader-subject bullet (base-entity axis-rotated-entity)
  ((affinity :initarg :affinity :accessor affinity))
  (:default-initargs
   :texture (asset 'shootman 'bullet)
   :size (vec2 16 16)
   :vertex-array (asset 'shootman '16x)
   :affinity 'bullet
   :axis +vz+))

(define-handler (bullet tick) (ev)
  (nv+ (location bullet) (vel bullet))
  (for:for ((entity over *loop*))
    (when (and (not (eql bullet entity))
               (not (typep entity (affinity bullet)))
               (typep entity 'solid))
      (let ((hit (test-collision bullet entity)))
        (when hit
          (hit bullet entity hit)
          (return))))))

(defmethod test-collision ((a bullet) (b base-entity))
  (let ((hit (test-segment-vs-aabb
              (vxy (location a))
              (v+ (vxy (vel a)) (vxy (vel b)))
              (vxy (location b))
              (v/ (size b) 2))))
    (when hit
      (setf (hit-a hit) a (hit-b hit) b)
      hit)))

(defmethod test-collision ((a bullet) (b wall))
  (let ((hit (test-segment-vs-aabb
              (vxy (location a))
              (vxy (vel a))
              (vxy (location b))
              (v/ (size b) 2))))
    (when hit
      (setf (hit-a hit) a (hit-b hit) b)
      hit)))

(defmethod hit (target by hit))

(defmethod hit ((bullet bullet) (solid solid) hit)
  (leave bullet *loop*))

(define-shader-subject gun-carrier (game-entity)
  ((gun :initarg :gun :accessor gun)))

(defmethod load progn ((gun-carrier gun-carrier))
  (load (gun gun-carrier)))

(defmethod register-object-for-pass :after (pass (gun-carrier gun-carrier))
  (register-object-for-pass pass (gun gun-carrier)))

(defmethod paint ((gun-carrier gun-carrier) target)
  (when (< (vx (direction gun-carrier)) 0)
    (scale-by -1 1 1))
  (call-next-method))

(defmethod paint :after ((gun-carrier gun-carrier) target)
  (paint (gun gun-carrier) target))

(defmethod (setf direction) :after (value (gun-carrier gun-carrier))
  (let ((dir (direction gun-carrier)))
    (setf (angle (gun gun-carrier))
          (atan (vy dir) (if (< (vx dir) 0) (- (vx dir)) (vx dir))))))

(defmethod look-at ((gun-carrier gun-carrier) target)
  (let ((dir (v- target (location gun-carrier))))
    (setf (direction gun-carrier) dir)))

(defmethod shoot ((gun-carrier gun-carrier) &key)
  (shoot (gun gun-carrier)
         :from (location gun-carrier)
         :direction (direction gun-carrier)
         :affinity (if (typep gun-carrier 'enemy) 'enemy 'player)))


(define-shader-subject laser-gun (basic-gun)
  ()
  (:default-initargs
   :bullet-type 'laser
   :bullet-velocity 5
   :tile (vec 1 0)
   :sound (pool-path 'shootman #p"laser.mp3")))

(define-shader-subject laser (bullet)
  ()
  (:default-initargs
   :tile (vec 1 1)))
