(in-package #:org.shirakumo.fraf.shootman)

(define-asset (shootman bullet) texture
    (#p"bullet.png")
  :mag-filter :nearest)

(define-asset (shootman gun) texture
    (#p"gun.png")
  :mag-filter :nearest)

(define-shader-subject gun (base-entity pivoted-entity)
  ()
  (:default-initargs
   :pivot (vec 13 3 0)
   :size (vec 16 16)
   :texture (asset 'shootman 'gun)
   :vertex-array (asset 'shootman '16x)))

(defmethod shoot ((gun gun) from direction affinity))

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
   :bullet-velocity 3
   :bullet-spread 0
   :bullet-arc 0
   :sound #p"shot.mp3"))

(defmethod shoot ((gun basic-gun) from direction affinity)
  ;; (setf (harmony:position (harmony-simple:play (sound gun) :sfx))
  ;;       (list (vx from) (vy from) (vz from)))
  (loop repeat (bullet-count gun)
        for phi from (- (/ (bullet-arc gun) 2))
        by (/ (bullet-arc gun) (bullet-count gun))
        for spread = (- (random (bullet-spread gun)) (/ (bullet-spread gun) 2))
        do (let ((dir (vrot direction +vz+ (deg->rad (+ phi spread)))))
             (unless (v= 0 dir) (nvunit dir))
             (make-instance (bullet-type gun)
                            :location (vcopy from)
                            :velocity (nv* dir (bullet-velocity gun))))))

(define-shader-subject bullet (base-entity)
  ((vel :initarg :velocity :accessor vel)
   (affinity :initarg :affinity :accessor affinity))
  (:default-initargs
   :texture (asset 'shootman 'bullet)
   :size (vec2 16 16)
   :vertex-array (asset 'shootman '16x)
   :affinity 'bullet))

(define-handler (bullet tick) (ev)
  (nv+ (location bullet) (vel bullet))
  (for:for ((entity over *loop*))
    (when (and (not (eql bullet entity))
               (not (typep entity (affinity bullet)))
               (typep entity 'solid))
      (let ((dist (v- (location bullet) (location entity)))
            (w (/ (vx (size entity)) 2))
            (h (/ (vy (size entity)) 2)))
        (when (and (<= (- w) (vx dist) w)
                   (<= (- h) (vy dist) h))
          (hit entity bullet)
          (return))))))

(defmethod hit (target by))

(defmethod hit ((wall wall) (bullet bullet))
  (leave bullet *loop*))

(define-shader-subject gun-carrier (game-entity)
  ((gun :initarg :gun :accessor gun)))

(defmethod load progn ((gun-carrier gun-carrier))
  (load (gun gun-carrier)))

(defmethod register-object-for-pass :after (pass (gun-carrier gun-carrier))
  (register-object-for-pass pass (gun gun-carrier)))

(defmethod paint ((gun-carrier gun-carrier) target)
  (when (eql :left (direction gun-carrier))
    (scale-by -1 1 1))
  (call-next-method))

(defmethod paint :after ((gun-carrier gun-carrier) target)
  (paint (gun gun-carrier) target))

(defmethod look-at ((gun-carrier gun-carrier) target)
  (let ((dir (nvunit (v- target
                         (location gun-carrier)))))
    (setf (angle (gun entity))
          (atan (vy dir) (if (eql :left (direction gun-carrier))
                             (- (vx dir))
                             (vx dir))))))

(defmethod shoot-at ((gun-carrier gun-carrier) target)
  (let ((direction (nvunit (v- target (location gun-carrier)))))
    (shoot (gun gun-carrier) (location gun-carrier) direction
           (if (typep gun-carrier 'enemy) 'enemy 'player))))
