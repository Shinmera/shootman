(in-package #:org.shirakumo.fraf.shootman)

(define-asset (shootman wall) texture
    (#p"wall.png")
  :mag-filter :nearest)

(define-asset (shootman splat) texture
    (#p"splat.png")
  :mag-filter :nearest)

(define-shader-subject splat (base-entity)
  ()
  (:default-initargs
   :texture (asset 'shootman 'splat)
   :angle (random (* 2 PI))))

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

(define-shader-subject ground (sprite-subject located-entity)
  ()
  (:default-initargs
   :vertex-array (asset 'shootman '32x)
   :texture (asset 'shootman 'wall)
   :tile (alexandria:random-elt
          (list (vec2 3 3) (vec2 3 3) (vec2 3 3) (vec2 3 2) (vec2 2 3)))))

(defun create-map-from-array (array scene)
  (destructuring-bind (h w) (array-dimensions array)
    (let ((cx (/ w 2/32)) (cy (/ h 2/32)))
      (dotimes (x w)
        (dotimes (y h)
          (let ((loc (vec (- (* x 32) cx) (- (* y 32) cy) 0)))
            (case (aref array y x)
              (-)
              (_
               (enter (make-instance 'ground :location loc) scene)
               (when (= 1 (random 50))
                 (enter (make-instance 'tomato :location (vcopy loc)) scene)))
              (T
               (enter (make-instance 'wall :location loc :orientation (aref array y x)) scene)))))))))
