(in-package #:org.shirakumo.fraf.shootman)

(defclass main (trial:main)
  ()
  (:default-initargs
   :clear-color (vec 0.1 0.1 0.1 1)))

(defmethod initialize-instance :after ((main main) &key)
  (harmony-simple:start)
  (setf (harmony:min-distance (harmony-simple:segment :sfx)) 32))

(defmethod finalize :after ((main main))
  (harmony-simple:stop))

(define-subject %camera (sidescroll-camera)
  ()
  (:default-initargs
   :name :camera
   :zoom 2.0))

(defmethod setup-perspective :after ((camera %camera) ev)
  (vsetf (location camera)
      (/ (width ev) 4)
      (/ (height ev) 4)
      -10))

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (create-map-from-array (load-map-file "long.map") scene)
      (enter (make-instance 'player) scene)
      (enter (make-instance '%camera :target (unit :player scene)) scene)))

  (maybe-reload-scene))

(progn
  (defmethod setup-pipeline ((main main))
    (let ((pipeline (pipeline main))
          (pass1 (make-instance 'render-pass)))
      (register pass1 pipeline)))

  (maybe-reload-scene))

(defun launch ()
  (trial:launch 'main))
