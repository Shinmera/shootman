(in-package #:org.shirakumo.fraf.shootman)

(define-asset (shootman tomato) texture
    (#p"tomato.png")
  :mag-filter :nearest)

(define-shader-subject enemy (gun-carrier solid)
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
   :texture (asset 'shootman 'tomato)
   :gun (make-instance 'basic-gun)))

(define-handler (tomato tick) (ev dt)
  (incf (timer tomato) dt)
  (let ((player (unit :player *loop*)))
    (look-at tomato (location player))
    (when (<= (cooldown tomato) (timer tomato))
      (shoot-at tomato (location player))
      (setf (timer tomato) 0))))
