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
          (when (= 1 (aref array y x))
            (enter (make-instance 'wall :location (vec (- (* x 32) cx) (- (* y 32) cy) 0)) scene)))))))

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (enter (make-instance 'flame) scene)
      (dotimes (i 15)
        (enter (make-instance 'tomato :location (vxy_ (vec2-random -320 320))) scene))
      (create-walls-from-array
       #2A((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
           (1 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
           (1 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
           (1 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
           (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
           (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
           (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
           (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
           (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
           (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
           (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
           (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
           (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
           (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
           (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
           (1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
           (1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
           (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
       scene)
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
