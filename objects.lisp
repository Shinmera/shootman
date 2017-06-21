(in-package #:org.shirakumo.fraf.shootman)

(define-pool shootman
  :base :shootman)

(define-asset (shootman bullet) texture
    (#p"bullet.png")
  :mag-filter :nearest)

(define-asset (shootman bullet2) texture
    (#p"bullet2.png")
  :mag-filter :nearest)

(define-asset (shootman flame) texture
    (#p"flame.png")
  :mag-filter :nearest)

(define-asset (shootman tomato) texture
    (#p"tomato.png")
  :mag-filter :nearest)

(define-asset (shootman wall) texture
    (#p"wall.png")
  :mag-filter :nearest)

(define-asset (shootman splat) texture
    (#p"splat.png")
  :mag-filter :nearest)

(define-shader-subject game-entity (sprite-subject located-entity axis-rotated-entity)
  ()
  (:default-initargs
   :size (vec 32 32)
   :axis +vz+))

(define-subject solid ()
  ())

(define-shader-subject splat (game-entity)
  ()
  (:default-initargs
   :texture (asset 'shootman 'splat)
   :angle (random (* 2 PI))))

(define-shader-subject bullet (game-entity)
  ((vel :initarg :velocity :accessor vel)
   (affinity :initarg :affinity :accessor affinity))
  (:default-initargs
   :texture (asset 'shootman 'bullet)
   :size (vec2 16 16)
   :affinity 'bullet))

(define-handler (bullet tick) (ev)
  (nv+ (location bullet) (vel bullet))
  (for:for ((entity over *loop*))
    (when (and (not (eql bullet entity))
               (not (typep entity (affinity bullet)))
               (typep entity 'game-entity))
      (let ((dist (v- (location bullet) (location entity)))
            (w (/ (vx (size entity)) 2))
            (h (/ (vy (size entity)) 2)))
        (when (and (<= (- w) (vx dist) w)
                   (<= (- h) (vy dist) h))
          (hit entity bullet)
          (return))))))

(defmethod hit (target by))

(define-shader-subject wall (game-entity solid)
  ()
  (:default-initargs
   :texture (asset 'shootman 'wall)))

(defmethod hit ((wall wall) (bullet bullet))
  (leave bullet *loop*))

(define-shader-subject enemy (game-entity solid)
  ((health :initarg :health :accessor health)
   (cooldown :initarg :cooldown :accessor cooldown)
   (timer :initarg :timer :initform 0 :accessor timer))
  (:default-initargs
   :health 3
   :cooldown 3
   :timer (random 3.0)))

(defmethod hit ((enemy enemy) (bullet bullet))
  (leave bullet *loop*)
  (decf (health enemy))
  (when (<= (health enemy) 0)
    (leave enemy *loop*)
    (enter (load (make-instance 'splat :location (location enemy))) *loop*)))

(define-shader-subject tomato (enemy)
  ()
  (:default-initargs
   :texture (asset 'shootman 'tomato)))

(define-handler (tomato tick) (ev dt)
  (incf (timer tomato) dt)
  (when (<= (cooldown tomato) (timer tomato))
    (shoot-at tomato (location (unit :player *loop*))
              :texture (asset 'shootman 'bullet2)
              :speed 2)
    (setf (timer tomato) 0)))

(define-shader-subject flame (animated-sprite-subject game-entity)
  ((vel :initform (vec 0 0 0) :accessor vel)
   (health :initarg :health :accessor health))
  (:default-initargs
   :texture (asset 'shootman 'flame)
   :animations '((0.5 3)
                 (0.5 4 :loop-to 3))
   :animation 0
   :name :player
   :health 6))

(define-handler (flame tick) (ev)
  (when (and (= 1 (animation flame))
             (= 3 (frame flame)))
    (maybe-reload-scene))
  
  (let ((moving NIL))
    (cond ((retained 'key :a)
           (setf moving T)
           (setf (vx (vel flame)) -1.6))
          ((retained 'key :d)
           (setf moving T)
           (setf (vx (vel flame)) +1.6))
          (T
           (setf (vx (vel flame)) 0)))
    (cond ((retained 'key :w)
           (setf moving T)
           (setf (vy (vel flame)) -1.6))
          ((retained 'key :s)
           (setf moving T)
           (setf (vy (vel flame)) +1.6))
          (T
           (setf (vy (vel flame)) 0)))

    (for:for ((entity over *loop*))
      (when (and (not (eql flame entity))
                 (typep entity 'solid))
        (let ((dist (v- (v+ (location flame) (vel flame)) (location entity)))
              (w (/ (+ (vx (size entity)) (vx (size flame))) 2))
              (h (/ (+ (vy (size entity)) (vy (size flame))) 2)))
          (when (and (<= (- w) (vx dist) w)
                     (<= (- h) (vy dist) h))
            (hit flame entity)
            (return)))))

    (nv+ (location flame) (vel flame))))

(define-handler (flame mouse-press) (ev)
  (shoot-at flame (screen->vec (pos ev) (width *context*) (height *context*))))

(defun shoot-at (source target &key (spread 5) (speed 3) (texture (asset 'shootman 'bullet)))
  (let* ((direction (nvunit (v- target (location source))))
         (spread (deg->rad (- (random spread) (/ spread 2)))))
    (nvrot direction +vz+ spread)
    (enter (load (make-instance 'bullet :location (vcopy (location source))
                                        :velocity (nv* direction speed)
                                        :affinity (type-of source)
                                        :texture texture))
           *loop*)))

(defmethod hit ((flame flame) (bullet bullet))
  (leave bullet *loop*)
  (decf (health flame))
  (when (<= (health flame) 0)
    (setf (animation flame) 1)))

(defmethod hit ((flame flame) (wall wall))
  (setf (vx (vel flame)) 0)
  (setf (vy (vel flame)) 0))
