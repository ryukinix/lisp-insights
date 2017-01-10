;; Common Lisp Script
;; Manoel Vilela

;; 2D Vector Abstraction

(setf (symbol-function 'make-vector) #'cons)
(setf (symbol-function 'xcor) #'car)
(setf (symbol-function 'ycor) #'cdr)

(defun +vect (u v)
  (make-vector (+ (xcor u)
                  (xcor v))
               (+ (ycor u)
                  (ycor v))))

(defun scale (s v)
  (make-vector (* s (xcor v))
               (* s (ycor v))))


;; Segment on Plane Abstraction

(setf (symbol-function 'make-seg) #'cons)
(setf (symbol-function 'seg-start) #'car)
(setf (symbol-function 'seg-end) #'cdr)

(defun segment-length (segment)
  (let* ((x (- (xcor (seg-start segment))
               (xcor (seg-end segment))))
         (y (- (ycor (seg-start segment))
               (ycor (seg-end segment)))))
        (sqrt (+ (* x x)
                 (* y y)))))

;; Data can be grouped using cons and a
;; general construction is a List

;; List construction
(cons 1 (cons 2 (cons 3 nil))) ;; => (1 2 3)
(list 1 2 3) ;; => 1 2 3

;; And accessed using car and cdr

(defparameter 1-to-4 (list 1 2 3 4))
(car 1-to-4) ;; 1
(cdr 1-to-4) ;; (2 3 4)
(car (cdr 1-to-4)) ;; (2)

;; Recursive data is useful for recursive procedures
;; We will implement a naive implementation of function map and for-each

(defun naive-map (p l)
  "Apply the p procedure at each element of list l
  and return a list"
  (if (null l)
      nil
      (cons (funcall p (car l))
            (naive-map p (cdr l)))))

(naive-map #'1+ 1-to-4)
(mapcar (lambda (x) (+ x 1)) 1-to-4 ) ;; default implementation

(defun for-each (p l)
  "Apply the p procedure at each element of list and
  return *done*"
  (if (null l)
      '*done*
      (progn (funcall p (car l))
             (for-each p (cdr l)))))

(for-each #'print 1-to-4)

;; Second Part: Peter's Language (DSL) about images

;; primitive: picture

;; rectangle
;; (make-rect)
;; (horiz)
;; (vert)
;; (origin)

(defun make-rect (origin horiz vert)
  (cons origin (cons horiz vert)))

(setf (symbol-function 'origin) #'car)
(setf (symbol-function 'horiz) #'cadr)
(setf (symbol-function 'vert) #'cddr)

(defun coord-map (rect)
  (lambda (point)
    (+vect (+vect (scale (xcor point)
                         (horiz rect))
                  (scale (ycor point)
                         (vert rect)))
           (origin rect))))

(defun make-picture (seglist)
  (lambda (rect)
    (for-each
      (lambda (s)
        (drawline
          ((coord-map rect) (seg-start s))
          ((coord-map rect) (seg-end s))))
      seglist)))

(defun beside (p1 p2 a)
  (lambda (rect)
    (funcall p1 (make-rect
                  (origin rect)
                  (scale a (horiz rect))
                  (vert rect)))
    (funcall p2 (make-rect
                 (+vect (origin rect)
                        (scale a (horiz rect)))
                 (scale (- 1 a) (horiz rect))
                 (vert rect)))))


(defun rotate90 (pict)
  (lambda (rect)
    (funcall pict (make-rect (+vect (origin rect)
                                    (horiz rect))
                             (vert rect)
                             (scale -1 (horiz rect))))))

(defun right-push (p n a)
  (if (= n 0)
      p
      (beside p (right-push p (1- n) a) a)))


;; this is a example how can be powerful writing a domain-specific-language
;; instead breaking your big task and a tree of tasks.
