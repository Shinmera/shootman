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

(progn
  (defmethod setup-scene ((main main))
    (let ((scene (scene main)))
      (create-map-from-array
       #2A((n< n  n  n  n  n  e< -  n< n  n  n  n  n  n  n  n  n  n  n  n  n  e<)
           (w  I  I  I  I  I  e  -  w  I  I  I  I  I  I  I  I  I  I  I  I  I  e )
           (w  _  _  _  _  _  e  -  w  _  _  _  _  _  _  _  _  _  _  _  _  _  e )
           (w  _  _  _  _  _  e  -  w  _  _  _  _  _  _  _  _  _  _  _  _  _  e )
           (w  _  _  _  _  _  e  -  w  _  _  _  _  _  _  _  _  _  _  _  _  _  e )
           (w  _  _  _  _  _  e> n  n> _  _  _  _  _  _  _  _  _  _  _  _  _  e )
           (w  _  _  _  _  _  I  I  I  _  _  _  _  _  _  _  _  _  _  _  _  _  e )
           (w  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e )
           (w  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e )
           (w  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e )
           (w  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  e )
           (w  _  _  _  _  _  _  _  _  _  _  _  _  s> s  s  w> _  _  _  _  _  e )
           (w  _  _  _  _  _  _  _  _  _  _  _  _  e  -  -  w  _  _  _  _  _  e )
           (w< s  s  s  w> _  _  _  _  _  _  _  _  e  -  -  w  _  _  _  _  _  e )
           (-  -  -  -  w  _  _  _  _  _  _  _  _  e  -  -  w  _  _  _  _  _  e )
           (-  -  -  -  w  _  _  _  _  _  _  _  _  e  -  -  w  _  _  _  _  _  e )
           (-  -  -  -  w  _  _  _  _  _  _  _  _  e  -  -  w  _  _  _  _  _  e )
           (-  -  -  -  w< s  s  s  s  s  s  s  s  s< -  -  w< s  s  s  s  s  s<))
       scene)
      (enter (make-instance 'player) scene)
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
