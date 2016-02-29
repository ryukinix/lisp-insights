;; Common Lisp Script
;; Manoel Vilela -- but i not have wrote that script

(ql:quickload 'lispbuilder-sdl)
(ql:quickload 'lispbuilder-sdl-gfx)
(ql:quickload 'alexandria)
 
(defparameter *world* (make-array '(100 100) :element-type 'bit))
 
;; initialize
(defun init-world! (world)
  (loop for i from 0 to (1- (array-dimension world 0)) do
    (loop for j from 0 to (1- (array-dimension world 1)) do
      (setf (aref world i j) (if (zerop (random 7)) 1 0)))))
 
(defun count-neighboring-individual (i j world)
  (let ((next-i (if (= i (1- (array-dimension world 0))) 0 (1+ i)))
    (prev-i (if (= i 0) (1- (array-dimension world 0)) (1- i)))
    (next-j (if (= j (1- (array-dimension world 1))) 0 (1+ j)))
    (prev-j (if (= j 0) (1- (array-dimension world 1)) (1- j))))
    (+ (aref world prev-i prev-j) (aref world prev-i j) (aref world prev-i next-j)
       (aref world i prev-j) (aref world i next-j)
       (aref world next-i prev-j) (aref world next-i j) (aref world next-i next-j))))
 
;; return next generation world
(defun update-next-generation (world)
  (let ((next-world (alexandria:copy-array  world)))
    (loop for i from 0 to (1- (array-dimension world 0)) do
      (loop for j from 0 to (1- (array-dimension world 1)) do
    (cond ((and (zerop (aref world i j)) ; birth
            (= (count-neighboring-individual i j world) 3))
           (setf (aref next-world i j) 1))
          ((and (= (aref world i j) 1)   ; die by under-population or overcrowding
            (or (<= (count-neighboring-individual i j world) 1)
            (>= (count-neighboring-individual i j world) 4)))
           (setf (aref next-world i j) 0)))))
    next-world))
 
(defun life ()
  (sdl:with-init ()
    (sdl:window 400 400) ; size of window
    (setf (sdl:frame-rate) 30) ; set frame-rate 60fps
    (init-world! *world*)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
         (setf *world* (update-next-generation *world*))
         (loop for i from 0 to (1- (array-dimension *world* 0)) do
           (loop for j from 0 to (1- (array-dimension *world* 1)) do
         (if (= (aref *world* i j) 0)
             (sdl-gfx:draw-box (sdl:rectangle :x (* i 4) :y (* j 4) :w 4 :h 4)
                                       :color sdl:*black*)
             (sdl-gfx:draw-box (sdl:rectangle :x (* i 4) :y (* j 4) :w 4 :h 4)
                                       :color sdl:*white*))))
         (sdl:update-display)))))
 
(life)