(in-package #:org.shirakumo.fraf.shootman)

(define-action shoot ()
  (mouse-press (one-of button :left))
  (gamepad-press (one-of button :a :r1)))

(define-action dash ()
  (mouse-press (one-of button :right))
  (key-press (one-of key :space))
  (gamepad-press (one-of button :l1)))

(define-asset (shootman flame) texture
    (#p"flame.png")
  :mag-filter :nearest)

(define-shader-subject player (animated-sprite-subject gun-carrier)
  ((move-vel :initform (vec 0 0 0) :accessor move-vel)
   (health :initarg :health :accessor health)
   (movement :initarg :movement :initform :keyboard :accessor movement)
   (status :initform NIL :accessor status))
  (:default-initargs
   :texture (asset 'shootman 'flame)
   :animations '((0.5 3)
                 (0.5 4 :loop-to 3)
                 (0.4 8 :loop-to 7))
   :animation 0
   :name :player
   :health 6
   :gun (make-instance 'basic-gun)))

(define-handler (player gamepad-press) (ev button)
  (when (eql :home button)
    (setf (movement player) (case (movement player)
                              (:keyboard :gamepad)
                              (:gamepad :keyboard)))))

(define-handler (player key-press) (ev key)
  (when (eql :keyboard (movement player))
    (case key
      (:a (setf (vx (move-vel player)) (- 1.6)))
      (:d (setf (vx (move-vel player)) (+ 1.6)))
      (:w (setf (vy (move-vel player)) (- 1.6)))
      (:s (setf (vy (move-vel player)) (+ 1.6))))))

(define-handler (player key-release) (ev key)
  (when (eql :keyboard (movement player))
    (case key
      (:a (setf (vx (move-vel player)) (max 0 (vx (move-vel player)))))
      (:d (setf (vx (move-vel player)) (min 0 (vx (move-vel player)))))
      (:w (setf (vy (move-vel player)) (max 0 (vy (move-vel player)))))
      (:s (setf (vy (move-vel player)) (min 0 (vy (move-vel player))))))))

(define-handler (player mouse-move) (ev pos)
  (when (eql :keyboard (movement player))
    (look-at player (screen->vec pos (width *context*) (height *context*)))))

(define-handler (player gamepad-move) (ev axis old-pos pos)
  (when (eql :gamepad (movement player))
    (case axis
      (:left-h
       (if (< (abs pos) 0.2)
           (setf (vx (move-vel player)) 0)
           (setf (vx (move-vel player)) (* pos 2.0))))
      (:left-v
       (if (< (abs pos) 0.2)
           (setf (vy (move-vel player)) 0)
           (setf (vy (move-vel player)) (* pos 2.0))))
      (:right-h
       (when (< 0.2 (abs pos))
         (setf (vx (direction player)) pos)
         (setf (direction player) (nvunit (direction player)))))
      (:right-v
       (when (< 0.2 (abs pos))
         (setf (vy (direction player)) pos)
         (setf (direction player) (nvunit (direction player))))))))

(define-handler (player tick) (ev)
  (case (status player)
    (:dead
     (when (= 3 (frame player))
       (maybe-reload-scene)))
    (:dash
     (unless (v= 0 (vel player))
       (setf (vel player) (nv* (nvunit (vel player)) 5.0)))
     (when (= 7 (frame player))
       (setf (status player) NIL)
       (setf (animation player) 0)))
    (T
     (vsetf (vel player)
         (vx (move-vel player))
         (vy (move-vel player)))))

  (let ((nearest-hit NIL))
    (loop repeat 2
          do (setf nearest-hit NIL)
             (for:for ((entity over *loop*))
               (when (and (not (eql player entity))
                          (typep entity 'solid))
                 (let ((hit (test-collision player entity)))
                   (when (and hit (or (not nearest-hit)
                                      (< (hit-time hit) (hit-time nearest-hit))))
                     (setf nearest-hit hit)))))
          while nearest-hit
          do (hit (hit-a nearest-hit) (hit-b nearest-hit) nearest-hit)))

  (nv+ (location player) (vel player))
  (setf (harmony:location (harmony:segment :sfx harmony-simple:*server*))
        (list (vx (location player)) (vy (location player)) (vz (location player)))))

(define-handler (player shoot) (ev)
  (unless (status player)
    (shoot player)))

(define-handler (player dash) (ev)
  (unless (status player)
    (setf (status player) :dash)
    (setf (animation player) 2)
    (cond ((v/= 0 (vel player))
           (vsetf (vel player)
               (vx (move-vel player))
               (vy (move-vel player))))
          ((v/= 0 (direction player))
           (vsetf (vel player)
               (- (vx (direction player)))
               (- (vy (direction player)))))
          (T
           (vsetf (vel player)
               1
               0)))
    (setf (vel player)
          (nv* (nvunit (vel player)) 5.0))))

(defmethod hit :after ((bullet bullet) (player player) hit)
  (unless (eql (status player) :dash)
    (decf (health player))
    (when (<= (health player) 0)
      (setf (status player) :dead)
      (setf (animation player) 1))))

(defmethod hit ((player player) (wall wall) hit)
  (vsetf (location player) (round (vx (hit-pos hit))) (round (vy (hit-pos hit))) (vz (location player)))
  (vsetf (vel player) (vx (hit-vel hit)) (vy (hit-vel hit)) 0))
