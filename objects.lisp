(in-package #:org.shirakumo.fraf.shootman)

(define-pool shootman
  :base :shootman)

(define-asset (shootman 32x) mesh
    ((make-rectangle 32 32)))

(define-asset (shootman 16x) mesh
    ((make-rectangle 16 16)))

(define-shader-subject base-entity (sprite-subject located-entity axis-rotated-entity)
  ()
  (:default-initargs
   :size (vec 32 32)
   :vertex-array (asset 'shootman '32x)
   :axis +vz+))

(define-shader-subject game-entity (base-entity)
  ((direction :initarg :direction :initform :right :accessor direction)))

(define-subject solid ()
  ())
