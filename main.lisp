(in-package #:org.shirakumo.fraf.shootman)

(defclass main (trial:main)
  ()
  (:default-initargs
   :clear-color (vec 0.1 0.1 0.1 1)))

(defun create-walls-from-array (array scene)
  (destructuring-bind (h w) (array-dimensions array)
    (let ((cx (/ w 2/32)) (cy (/ h 2/32)))
      (dotimes (x w)
        (dotimes (y h)
          (let ((loc (vec (- (* x 32) cx) (- (* y 32) cy) 0)))
            (case (aref array y x)
              (-)
              (_
               (enter (make-instance 'ground :location loc) scene))
              (T
               (enter (make-instance 'wall :location loc :orientation (aref array y x)) scene)))))))))

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (create-walls-from-array
       #2A((n< n  n  n  n  n  e< n< n  n  n  n  n  n  n  n  n  n  n  n  n  n  e<)
           (w  I  I  I  I  I  e  w  I  I  I  I  I  I  I  I  I  I  I  I  I  I  e)
           (w  _  _  _  _  _  e  w  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e)
           (w  _  _  _  _  _  e> n> _  _  _  _  _  _  _  _  _  _  _  _  _  _  e)
           (w  _  _  _  _  _  I  I  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e)
           (w  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e)
           (w  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e)
           (w  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e)
           (w  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e)
           (w  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e)
           (w  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e)
           (w  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e)
           (w  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e)
           (w  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e)
           (w  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e)
           (w< s  w> _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e)
           (-  -  w  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e)
           (-  -  w< s  s  s  s  s  s  s  s  s  s  s  s  s  s  s  s  s  s  s  s<))
       scene)
      (enter (make-instance 'flame) scene)
      (dotimes (i 15)
        (enter (make-instance 'tomato :location (vxy_ (vec2-random -320 320))) scene))
      (enter (make-instance 'sidescroll-camera :location (vec 200 150 -10)
                                               :name :camera
                                               :zoom 2.0
                                               :target (unit :player scene)) scene)))

  (maybe-reload-scene))

(progn
  (defmethod setup-pipeline ((main main))
    (let ((pipeline (pipeline main))
          (pass1 (make-instance 'render-pass)))
      (register pass1 pipeline)))

  (maybe-reload-scene))

(defun launch ()
  (trial:launch 'main))
