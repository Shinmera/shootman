(in-package #:org.shirakumo.fraf.shootman)

(define-asset (shootman bullet) texture
    (#p"bullet.png")
  :mag-filter :nearest)

(define-asset (shootman bullet2) texture
    (#p"bullet2.png")
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
  (let* ((direction (nvunit (v- target (location gun-carrier))))
         (spread (deg->rad (- (random spread) (/ spread 2)))))
    (nvrot direction +vz+ spread)
    (shoot (gun gun-carrier) (location gun-carrier) direction
           (if (typep gun-carrier 'enemy) 'enemy 'player))))
