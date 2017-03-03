;; Common Lisp Script
;; Manoel Vilela

(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)
(defparameter *reproduction-energy* 200)

;; PLANTS WITH (X . Y) KEY COORDINATES HASH-MAP
(defparameter *plants* (make-hash-table :test #'equal))
;; need to use #'equal here because the default #'eq
;; doesn't works fine to compare cons cells.


(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width))
                   (+ top (random height)))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))


;; x y => cartesian location
;; energy => days left of life
;; dir => direction as a number between 0 and 7.
;;               0 1 2
;;               7 * 3
;;               6 5 4
;; genes => list of eight elements
;; the list of probabilities bounds for decide the daily direction
;; greater number, more likely to be chosen
(defstruct animal x y energy dir genes)


(defparameter *animals*
  (list (make-animal :x (ash *width* -1)
                     :y (ash *height* -1)
                     :energy 100
                     :dir 0
                     :genes (loop repeat 8
                                  collecting (1+ (random 10))))))

(defun move (animal)
  (let ((dir (animal-dir animal))
        (x (animal-x animal))
        (y (animal-y animal)))
        (setf (animal-x animal) (mod (+ x
                                        (cond ((and (>= dir 2)
                                                    (< dir 5)) 1)
                                              ((or (= dir 1)
                                                   (= dir 5)) 0)
                                              (t -1))
                                        *width*)
                                     *width*))
        (setf (animal-y animal) (mod (+ y
                                        (cond ((and (>= dir 0)
                                                    (< dir 3)) 1)
                                              ((or (>= dir 4)
                                                   (< dir 7)) -1)
                                              (t 0))
                                        *height*)
                                     *height*))
        (decf (animal-energy animal))))

(defun turn (animal)
  (let ((x (random (apply #'+ (animal-genes animal)))))
    (labels ((angle (genes x)
               (let ((xnu (- x (car genes))))
                 (if (< xnu 0)
                     0
                     (1+ (angle (cdr genes) xnu))))))
      (setf (animal-dir animal)
            (mod (+ (animal-dir animal)
                    (angle (animal-genes animal) x))
                 8)))))

(defun eat (animal)
  (let ((pos (cons (animal-x animal)
                   (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))


(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      (setf (animal-energy animal) (ash e -1))
      (let ((animal-nu (copy-structure animal))
            (genes (copy-list (animal-genes animal)))
            (mutation (random 8)))
        (setf (nth mutation genes)
              (max 1 (+ (nth mutation genes)
                        (random 3)
                        -1)))
        (setf (animal-genes animal-nu) genes)
        (push animal-nu *animals*)))))

(defun update-world ()
  (setf *animals* (remove-if (lambda (animal)
                               (<= (animal-energy animal) 0))
                             *animals*))
  (mapc (lambda (animal)
          (turn animal)
          (move animal)
          (eat animal)
          (reproduce animal))
        *animals*)
  (add-plants))

(defun draw-world ()
  (loop for y
        below *height*
        do (progn (fresh-line)
                  (princ "|")
                  (loop for x
                        below *width*
                        do (princ (cond ((some (lambda (animal)
                                                 (and (= (animal-x animal) x)
                                                      (= (animal-y animal) y)))
                                               *animals*)
                                         #\M)
                                        ((gethash (cons x y) *plants*) #\*)
                                        (t #\space))))
                  (princ "|"))))

(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") 'nil)
          (t (let ((x (parse-integer str :junk-allowed t)))
               (if x
                   (loop for i
                         below x
                         do (update-world)
                         if (zerop (mod i 1000))
                           do (princ #\.))
                   (update-world))
               (evolution))))))

(format t "At each step: 1. type a integer to let evolution for n days, 'quit' for exit and just RETURN to update.. ~%")
(format t "* => PLANT ~%")
(format t "M => ANIMAL ~%")
(evolution)
