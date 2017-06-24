(in-package #:org.shirakumo.fraf.shootman)

(define-asset (shootman wall) texture
    (#p"wall.png")
  :mag-filter :nearest)

(define-asset (shootman splat) texture
    (#p"splat.png")
  :mag-filter :nearest)

(define-shader-subject floor-drawable (sprite-subject)
  ()
  (:default-initargs
   :size (vec 32 32)
   :vertex-array (asset 'shootman '32x)
   :texture (asset 'shootman 'wall)))

(defclass wall (located-entity solid)
  ((size :initform (vec 32 32) :accessor size)))

(define-shader-subject ground (sprite-subject located-entity)
  ()
  (:default-initargs
   :name :ground
   :vertex-array NIL
   :texture NIL
   :tile NIL))

(defun create-map-from-array (array scene)
  (destructuring-bind (h w) (array-dimensions array)
    (let* ((pw (* w 32)) (ph (* h 32))
           (pipeline (make-instance 'scene-buffer))
           (ground (make-instance 'ground :size (vec pw ph)
                                          :vertex-array (change-class (make-rectangle pw ph) 'vertex-array)
                                          :texture (texture pipeline)))
           (drawable (make-instance 'floor-drawable)))
      (enter ground scene)
      (enter drawable pipeline)
      (load pipeline)
      (with-pushed-matrix ((projection-matrix) :zero)
        (orthographic-projection 0 pw 0 ph -1 1)
        (with-pushed-matrix ((view-matrix) :identity)
          (with-pushed-matrix ((model-matrix) :identity)
            (unwind-protect
                 (let ((cx (/ pw 2)) (cy (/ ph 2)))
                   (dotimes (x w)
                     (dotimes (y h)
                       (let ((loc (vec (- (* x 32) cx) (- (* y 32) cy) 0)))
                         (setf (location drawable) loc)
                         (case (aref array y x)
                           (-)
                           (_
                            (setf (tile drawable) (alexandria:random-elt
                                                   (list (vec2 3 2)
                                                         (vec2 2 3)
                                                         (vec2 3 3))))
                            (paint drawable pipeline))
                           (T
                            (setf (tile drawable) (ecase (aref array y x)
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
                                                    (I  (vec2 2 2))))
                            (enter (make-instance 'wall :location loc) scene)
                            (paint drawable pipeline))))))))))))))
