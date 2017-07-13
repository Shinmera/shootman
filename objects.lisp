(in-package #:org.shirakumo.fraf.shootman)

(define-pool shootman
  :base :shootman)

(define-asset (shootman 32x) mesh
    ((make-rectangle 32 32)))

(define-asset (shootman 16x) mesh
    ((make-rectangle 16 16)))

(define-shader-subject base-entity (sprite-subject located-entity axis-rotated-entity)
  ((vel :initarg :velocity :accessor vel))
  (:default-initargs
   :size (vec 32 32)
   :velocity (vec 0 0 0)
   :vertex-array (asset 'shootman '32x)
   :axis +vz+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass solid ()
    ()))

(define-shader-subject game-entity (base-entity solid)
  ((direction :initarg :direction :initform (vec 1 0) :accessor direction)))


