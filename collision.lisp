(in-package #:org.shirakumo.fraf.shootman)

(defun %sig (x)
  (if (< x 0.0s0) -1.0s0 1.0s0))

(defstruct hit
  (a NIL) (b NIL)
  (time 0.0s0 :type single-float)
  (normal (vec 0 0) :type vec2)
  (vel (vec 0 0) :type vec2)
  (pos (vec 0 0) :type vec2))

(defun test-segment-vs-aabb (seg-pos seg-vel aabb-pos aabb-size)aswda
  (declare (type vec2 seg-pos seg-vel aabb-pos aabb-size))
  (sb-int:with-float-traps-masked (:overflow :underflow :inexact)
    (let* ((scale (vec (if (= 0 (vx seg-vel)) most-positive-single-float (/ (vx seg-vel)))
                       (if (= 0 (vy seg-vel)) most-positive-single-float (/ (vy seg-vel)))))
           (sign (vapply seg-vel %sig))
           (near (v* (v- (v- aabb-pos (v* sign aabb-size)) seg-pos) scale))
           (far  (v* (v- (v+ aabb-pos (v* sign aabb-size)) seg-pos) scale)))
      (unless (or (< (vy far) (vx near))
                  (< (vx far) (vy near)))
        (let ((t-near (max (vx near) (vy near)))
              (t-far (min (vx far) (vy far))))
          (unless (or (<= 1 t-near)
                      (<= t-far 0))
            (let* ((time (alexandria:clamp t-near 0.0s0 1.0s0))
                   (normal (if (< (vy near) (vx near))
                               (vec (- (vx sign)) 0)
                               (vec 0 (- (vy sign)))))
                   (vel (v* seg-vel time)))
              (make-hit :time time
                        :normal normal
                        :vel vel
                        :pos (v+ seg-pos vel)))))))))

(defun test-aabb-vs-aabb (a-pos a-size b-pos b-size)
  (let* ((d (v- b-pos a-pos))
         (p (v- (v+ a-size b-size) (vabs d)))
         (s (vapply d %sig)))
    (unless (or (<= (vx p) 0)
                (<= (vy p) 0))
      (cond ((< (vx p) (vy p))
             (make-hit :normal (vec (vx s) 0)
                       :vel (vec (* (vx p) (vx s)) 0)
                       :pos (vec (+ (vx a-pos) (* (vx a-size) (vx s)))
                                 (vy b-pos))))
            (T
             (make-hit :normal (vec 0 (vy s))
                       :vel (vec 0 (* (vy p) (vy s)))
                       :pos (vec (vx b-pos)
                                 (+ (vy a-pos) (* (vy a-size) (vy s))))))))))

(defun test-sweep-aabb-vs-aabb (a-pos a-vel a-size b-pos b-vel b-size)
  (let ((vel (v+ a-vel b-vel)))
    (cond ((v= 0 vel)
           (test-aabb-vs-aabb a-pos a-size b-pos b-size))
          (T
           (test-segment-vs-aabb a-pos vel b-pos (v+ a-size b-size))))))

(defmethod test-collision ((a base-entity) (b base-entity))
  (let ((hit (test-sweep-aabb-vs-aabb
              (vxy (location a))
              (vxy (vel a))
              (v/ (size a) 2)
              (vxy (location b))
              (vxy (vel b))
              (v/ (size b) 2))))
    (when hit
      (setf (hit-a hit) a (hit-b hit) b)
      hit)))

(defmethod test-collision ((a base-entity) (b wall))
  (let ((hit (test-sweep-aabb-vs-aabb
              (vxy (location a))
              (vxy (vel a))
              (v/ (size a) 2)
              (vxy (location b))
              (vec 0 0)
              (v/ (size b) 2))))
    (when hit
      (setf (hit-a hit) a (hit-b hit) b)
      hit)))
