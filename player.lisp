(in-package #:org.shirakumo.fraf.shootman)

(define-asset (shootman flame) texture
    (#p"flame.png")
  :mag-filter :nearest)

(define-shader-subject player (animated-sprite-subject gun-carrier)
  ((vel :initform (vec 0 0 0) :accessor vel)
   (health :initarg :health :accessor health)
   (viewing :initform (vec 0 0) :accessor viewing))
  (:default-initargs
   :texture (asset 'shootman 'flame)
   :animations '((0.5 3)
                 (0.5 4 :loop-to 3))
   :animation 0
   :name :player
   :health 6
   :gun (make-instance 'basic-gun)))

(define-handler (player tick) (ev)
  (when (and (= 1 (animation player))
             (= 3 (frame player)))
    (maybe-reload-scene))
  
  (let ((moving NIL))
    (cond ((retained 'key :a)
           (setf moving T)
           (setf (direction player) :left)
           (setf (vx (vel player)) -1.6))
          ((retained 'key :d)
           (setf moving T)
           (setf (direction player) :right)
           (setf (vx (vel player)) +1.6))
          (T
           (setf (vx (vel player)) 0)))
    (cond ((retained 'key :w)
           (setf moving T)
           (setf (vy (vel player)) -1.6))
          ((retained 'key :s)
           (setf moving T)
           (setf (vy (vel player)) +1.6))
          (T
           (setf (vy (vel player)) 0)))

    (for:for ((entity over *loop*))
      (when (and (not (eql player entity))
                 (typep entity 'solid))
        (let ((dist (v- (v+ (location player) (vel player)) (location entity)))
              (w (/ (+ (vx (size entity)) (vx (size player))) 2))
              (h (/ (+ (vy (size entity)) (vy (size player))) 2)))
          (when (and (<= (- w) (vx dist) w)
                     (<= (- h) (vy dist) h))
            (hit player entity)
            (return)))))

    (nv+ (location player) (vel player))
    (look-at player (screen->vec (viewing player) (width *context*) (height *context*)))))

(define-handler (player mouse-press) (ev)
  (shoot-at player (screen->vec (viewing player) (width *context*) (height *context*))))

(define-handler (player mouse-move) (ev pos)
  (setf (viewing player) pos))

(defmethod hit ((player player) (bullet bullet))
  (leave bullet *loop*)
  (decf (health player))
  (when (<= (health player) 0)
    (setf (animation player) 1)))

(defmethod hit ((player player) (wall wall))
  (setf (vx (vel player)) 0)
  (setf (vy (vel player)) 0))
