(in-package #:org.shirakumo.fraf.shootman)

(define-asset (shootman wall) texture
    (#p"wall.png")
  :mag-filter :nearest)

(define-asset (shootman splat) texture
    (#p"splat.png")
  :mag-filter :nearest)

(define-shader-subject floor-drawable (sprite-subject located-entity)
  ()
  (:default-initargs
   :size (vec 32 32)
   :vertex-array (asset 'shootman '32x)
   :texture (asset 'shootman 'wall)))

(defclass wall (located-entity solid)
  ((size :initform (vec 32 32) :accessor size)
   (symb :initarg :symb :accessor symb)))

(defmethod print-object ((wall wall) stream)
  (print-unreadable-object (wall stream :type T)
    (format stream "~a ~a" (symb wall) (location wall))))

(define-shader-subject ground (vertex-subject textured-subject located-entity)
  ()
  (:default-initargs
   :name :ground
   :vertex-array NIL
   :texture NIL))

(defun symb->tile (symb)
  (ecase symb
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

(defun load-map-file (file)
  (with-open-file (stream (pool-path 'shootman file) :direction :input)
    (let* ((*package* #.*package*)
           (list (loop for line = (read-line stream NIL)
                       while (and line (string/= line ""))
                       collect (with-input-from-string (stream line)
                                 (loop for symbol = (read stream NIL)
                                       while symbol
                                       collect symbol)))))
      (make-array (list (length list)
                        (length (first list)))
                  :initial-contents list))))

(progn
  (defun create-map-from-array (array scene)
    (destructuring-bind (h w) (array-dimensions array)
      (let* ((pw (* w 32)) (ph (* h 32))
             (cx (/ pw 2)) (cy (/ ph 2))
             (buffer (make-instance 'scene-buffer :width pw :height ph
                                                  :texture-properties '(:min-filter :nearest :mag-filter :nearest))))
        (enter (make-instance 'ground :vertex-array (change-class (make-rectangle pw ph) 'vertex-array)
                                      :texture (texture buffer))
               scene)
        (with-pushed-matrix ((*projection-matrix* :zero)
                             (*view-matrix* :identity)
                             (*model-matrix* :identity))
          (orthographic-projection 0 pw 0 ph -10 10)
          (dotimes (x w)
            (dotimes (y h)
              (let ((loc (vec (+ 16 (* x 32)) (+ 16 (* y 32)) 0)))
                (case (aref array y x)
                  (-)
                  (_
                   (when (< (random 100) 2)
                     (enter (make-instance (alexandria:random-elt '(robot-a robot-b))
                                           :location (v- loc (vec cx cy 0)))
                            scene))
                   (enter (make-instance 'floor-drawable :location loc
                                                         :tile (alexandria:random-elt
                                                                (list (vec2 3 2)
                                                                      (vec2 2 3)
                                                                      (vec2 3 3))))
                          buffer))
                  (T
                   (enter (make-instance 'floor-drawable :location loc
                                                         :tile (symb->tile (aref array y x)))
                          buffer)
                   (enter (make-instance 'wall :location (v- loc (vec cx cy 0)) :symb (aref array y x))
                          scene))))))
          (load buffer)
          (paint buffer buffer)))))
  (maybe-reload-scene))
