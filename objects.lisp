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

(define-asset (shootman gun) texture
    (#p"gun.png")
  :mag-filter :nearest)

(define-asset (shootman 32x) mesh
    ((make-rectangle 32 32)))

(define-asset (shootman 16x) mesh
    ((make-rectangle 16 16)))

(defun update-gun (entity target)
  (let ((dir (nvunit (v- target
                         (location entity)))))
    (setf (angle (gun entity)) (atan (vy dir) (if (eql :left (direction entity))
                                                  (- (vx dir))
                                                  (vx dir))))))

(define-shader-subject base-entity (sprite-subject located-entity axis-rotated-entity)
  ()
  (:default-initargs
   :size (vec 32 32)
   :vertex-array (asset 'shootman '32x)
   :axis +vz+))

(define-shader-subject game-entity (base-entity)
  ((direction :initarg :direction :initform :right :accessor direction)
   (gun :initform (make-instance 'gun) :accessor gun)))

(defmethod load progn ((game-entity game-entity))
  (load (gun game-entity)))

(defmethod register-object-for-pass :after (pass (game-entity game-entity))
  (register-object-for-pass pass (gun game-entity)))

(defmethod paint ((game-entity game-entity) target)
  (when (eql :left (direction game-entity))
    (scale-by -1 1 1))
  (call-next-method))

(defmethod paint :after ((game-entity game-entity) target)
  (paint (gun game-entity) target))

(define-subject solid ()
  ())

(define-shader-subject gun (base-entity pivoted-entity)
  ()
  (:default-initargs
   :pivot (vec 13 3 0)
   :size (vec 16 16)
   :texture (asset 'shootman 'gun)
   :vertex-array (asset 'shootman '16x)))

(define-shader-subject splat (base-entity)
  ()
  (:default-initargs
   :texture (asset 'shootman 'splat)
   :angle (random (* 2 PI))))

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

(define-shader-subject wall (sprite-subject located-entity solid)
  ()
  (:default-initargs
   :texture (asset 'shootman 'wall)
   :size (vec 32 32)
   :vertex-array (asset 'shootman '32x)
   :orientation :n))

(defmethod shared-initialize :after ((wall wall) slots &key orientation)
  (setf (tile wall)
        (ecase orientation
          ((NIL) (tile wall))
          (n  (vec2 0 0))
          (e  (vec2 1 0))
          (s  (vec2 1 1))
          (w  (vec2 0 1))
          (n> (vec2 2 0))
          (e> (vec2 3 0))
          (s> (vec2 3 1))
          (w> (vec2 2 1))
          (n< (vec2 0 2))
          (e< (vec2 1 2))
          (s< (vec2 1 3))
          (w< (vec2 0 3))
          (I  (vec2 2 2)))))

(defmethod hit ((wall wall) (bullet bullet))
  (leave bullet *loop*))

(define-shader-subject ground (sprite-subject located-entity)
  ()
  (:default-initargs
   :vertex-array (asset 'shootman '32x)
   :texture (asset 'shootman 'wall)
   :tile (alexandria:random-elt
          (list (vec2 3 3) (vec2 3 3) (vec2 3 3) (vec2 3 2) (vec2 2 3)))))

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
  (let ((player (unit :player *loop*)))
    (update-gun tomato (location player))
    (when (<= (cooldown tomato) (timer tomato))
      (shoot-at tomato (location player)
                :texture (asset 'shootman 'bullet2)
                :speed 2)
      (setf (timer tomato) 0))))

(define-shader-subject flame (animated-sprite-subject game-entity)
  ((vel :initform (vec 0 0 0) :accessor vel)
   (health :initarg :health :accessor health)
   (viewing :initform (vec 0 0) :accessor viewing))
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
           (setf (direction flame) :left)
           (setf (vx (vel flame)) -1.6))
          ((retained 'key :d)
           (setf moving T)
           (setf (direction flame) :right)
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

    (nv+ (location flame) (vel flame))
    (update-gun flame (screen->vec (viewing flame) (width *context*) (height *context*)))))

(define-handler (flame mouse-press) (ev)
  (shoot-at flame (screen->vec (viewing flame) (width *context*) (height *context*))
            :speed 5 :spread 10))

(define-handler (flame mouse-move) (ev pos)
  (setf (viewing flame) pos))

(defun shoot-at (source target &key (spread 5) (speed 3) (texture (asset 'shootman 'bullet)))
  (let* ((direction (nvunit (v- target (location source))))
         (spread (deg->rad (- (random spread) (/ spread 2)))))
    (nvrot direction +vz+ spread)
    (enter (load (make-instance 'bullet :location (v+ (location source) (v* direction 16))
                                        :velocity (nv* direction speed)
                                        :affinity (type-of source)
                                        :texture texture
                                        :angle (atan (vy direction) (vx direction))))
           *loop*)))

(defmethod hit ((flame flame) (bullet bullet))
  (leave bullet *loop*)
  (decf (health flame))
  (when (<= (health flame) 0)
    (setf (animation flame) 1)))

(defmethod hit ((flame flame) (wall wall))
  (setf (vx (vel flame)) 0)
  (setf (vy (vel flame)) 0))
