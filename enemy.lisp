(in-package #:org.shirakumo.fraf.shootman)

(define-asset (shootman enemy) texture
    (#p"enemy.png")
  :mag-filter :nearest)

(define-shader-subject enemy (animated-sprite-subject gun-carrier)
  ((health :initarg :health :accessor health)
   (cooldown :initarg :cooldown :accessor cooldown)
   (timer :initarg :timer :initform 0 :accessor timer))
  (:default-initargs
   :health 3
   :cooldown 3
   :timer (random 3.0)
   :texture (asset 'shootman 'enemy)
   :animations '((0.5 2)
                 (0.5 2))))

(defmethod hit :after ((bullet bullet) (enemy enemy) hit)
  (decf (health enemy))
  (when (<= (health enemy) 0)
    (leave enemy *loop*)
    ;; (enter (load (make-instance 'splat :location (location enemy))) *loop*)
    ))

(defmethod movement-logic ((enemy enemy) player))

(define-handler (enemy tick) (ev dt)
  (incf (timer enemy) dt)
  (let ((player (unit :player *loop*)))
    (look-at enemy (v+ (location player) (vel player)))
    (when (<= (cooldown enemy) (timer enemy))
      (shoot enemy)
      (setf (timer enemy) 0))
    (movement-logic enemy player))
  (nv+ (location enemy) (vel enemy)))

(define-shader-subject tracking-enemy (enemy)
  ())

(defmethod movement-logic ((enemy tracking-enemy) player)
  (let ((dir (vxy (v- (location player) (location enemy)))))
    (if (< 96 (vlength dir))
        (unless (for:for ((entity over *loop*))
                  (when (typep entity 'wall)
                    (let ((hit (test-segment-vs-aabb
                                (vxy (location enemy))
                                dir
                                (vxy (location entity))
                                (v+ (v/ (size enemy) 2)
                                    (v/ (size entity) 2)))))
                      (when hit
                        (vsetf (vel enemy) 0 0)
                        (return T)))))
          (nv* (nvunit dir) 0.5)
          (vsetf (vel enemy) (vx dir) (vy dir)))
        (vsetf (vel enemy) 0 0))))

(define-shader-subject robot-a (tracking-enemy)
  ()
  (:default-initargs
   :animation 0
   :gun (make-instance 'basic-gun :bullet-velocity 3)))

(define-shader-subject robot-b (tracking-enemy)
  ()
  (:default-initargs
   :animation 1
   :cooldown 10
   :gun (make-instance 'basic-gun :bullet-velocity 2
                                  :bullet-count 3
                                  :bullet-arc 15
                                  :tile (vec 2 0))))
