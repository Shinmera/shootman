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
  ((vel :initform (vec 0 0 0) :accessor vel)
   (svel :initform (vec 0 0 0) :accessor svel)
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
      (:a (setf (vx (vel player)) (- 1.6)))
      (:d (setf (vx (vel player)) (+ 1.6)))
      (:w (setf (vy (vel player)) (- 1.6)))
      (:s (setf (vy (vel player)) (+ 1.6))))))

(define-handler (player key-release) (ev key)
  (when (eql :keyboard (movement player))
    (case key
      (:a (setf (vx (vel player)) (max 0 (vx (vel player)))))
      (:d (setf (vx (vel player)) (min 0 (vx (vel player)))))
      (:w (setf (vy (vel player)) (max 0 (vy (vel player)))))
      (:s (setf (vy (vel player)) (min 0 (vy (vel player))))))))

(define-handler (player mouse-move) (ev pos)
  (when (eql :keyboard (movement player))
    (look-at player (screen->vec pos (width *context*) (height *context*)))))

(define-handler (player gamepad-move) (ev axis old-pos pos)
  (when (eql :gamepad (movement player))
    (case axis
      (:left-h
       (if (< (abs pos) 0.2)
           (setf (vx (vel player)) 0)
           (setf (vx (vel player)) (* pos 2.0))))
      (:left-v
       (if (< (abs pos) 0.2)
           (setf (vy (vel player)) 0)
           (setf (vy (vel player)) (* pos 2.0))))
      (:right-h
       (when (< 0.2 (abs pos))
         (setf (vx (direction player)) pos)
         (setf (direction player) (nvunit (direction player)))))
      (:right-v
       (when (< 0.2 (abs pos))
         (setf (vy (direction player)) pos)
         (setf (direction player) (nvunit (direction player))))))))

(define-handler (player tick) (ev)
  (let ((vel (case (status player)
               (:dash (svel player))
               (T (vel player)))))
    ;; Handle death
    (when (and (= 1 (animation player))
               (= 3 (frame player)))
      (maybe-reload-scene))
    ;; Handle dash end
    (when (and (= 2 (animation player))
               (= 7 (frame player)))
      (setf (status player) NIL)
      (setf (animation player) 0))

    (for:for ((entity over *loop*))
      (when (and (not (eql player entity))
                 (typep entity 'solid))
        (let ((dist (v- (v+ (location player) vel) (location entity)))
              (w (/ (+ (vx (size entity)) (vx (size player))) 2))
              (h (/ (+ (vy (size entity)) (vy (size player))) 2)))
          (when (and (<= (- w) (vx dist) w)
                     (<= (- h) (vy dist) h))
            (hit player entity)
            (return)))))

    (nv+ (location player) vel)
    (setf (harmony:location (harmony:segment :sfx harmony-simple:*server*))
          (list (vx (location player)) (vy (location player)) (vz (location player))))))

(define-handler (player shoot) (ev)
  (unless (status player)
    (shoot player)))

(define-handler (player dash) (ev)
  (unless (status player)
    (setf (status player) :dash)
    (setf (animation player) 2)
    (cond ((= 0 (vlength (vel player)))
           (vsetf (svel player)
               (- (vx (direction player)))
               (- (vy (direction player)))))
          (T
           (vsetf (svel player)
               (vx (vel player))
               (vy (vel player)))))
    (setf (svel player) (nv* (nvunit (svel player)) 5.0))))

(defmethod hit ((player player) (bullet bullet))
  (unless (eql (status player) :dash)
    (leave bullet *loop*)
    (decf (health player))
    (when (<= (health player) 0)
      (setf (animation player) 1))))

(defmethod hit ((player player) (wall wall))
  (vsetf (vel player) 0 0 0)
  (vsetf (svel player) 0 0 0))
