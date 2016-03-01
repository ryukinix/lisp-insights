;; Common Lisp Script
;; Manoel Vilela


;; Here we used cons to construct pair of things
;; that structures are also called 'list structures'
;; after all, list are essentially recursively cons
;; (list 1 2 3) → (cons 1 (cons 2 (cons 3 nil)))


#| Map of abstraction for: Rational Numbers

   Usage →  Rational numbers → Pairs of nubmers
     ↓         ↓                    ↓
    +rat      make-rat           cons
    *rat      numer              car
    /rat      denom              cdr
    -rat      inverse-rat        gcd
 |#


;; i don't use gcd as name of that procedure
;; because clisp have a reserved name-space locked as macro for gcd
;; what are funny... because, why sucks gcd is a macro?!
(defun my-gcd(a b)
  (if (= b 0)
      a
      (my-gcd b (mod a b))))

;; rational numbers (re-implementations)
(defun make-rat (n d)
  (let ((g (my-gcd n d)))
    (cons (/ n g)
          (/ d g))))

(defun numer (r)
  (car r))

(defun denom (r)
  (cdr r))

(defun inverse-rat (x)
  (make-rat (denom x) (numer x)))

(defun +rat (x y)
  (make-rat (+ (* (numer x) (denom y)) 
               (* (numer y) (denom y)))
            (* (denom x) (denom y))))

(defun -rat (x y)
  (+rat x (make-rat (- (numer y)) (denom y))))

(defun *rat (x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defun /rat (x y)
  (*rat x (inverse-rat y)))


#| Map of abstraction fir: Segments and Vectors

  Segments → Vectors → Pairs of nubmers
  ↓            ↓                 ↓
  make-seg    make-vector       cons
  seg-start   xcor              car
  seg-end     ycor              cdr

 |#


;; vectors bi-dimensional
;; whose x y are numbers
(defun make-vector (x y) (cons x y))

(defun xcor (v) (car v))

(defun ycor (v) (cdr v))

;; whose p q are vectors
(defun make-seg (p q) (cons p q))

(defun seg-start (s) (car s))

(defun seg-end (s) (cdr s))

(defun average (x y)
  (/ (+ x y) 2))

(defun midpoint (s)
  (let ((a (seg-start s))
        (b (seg-end s)))
    (make-vector
      (average (xcor a) (xcor b))
      (average (ycor b) (ycor b)))))

(defun segment-length (s)
  (flet ((square (x) (* x x)))
         (let ((dx (- (xcor (seg-end s))
                 (xcor (seg-start s))))
          (dy (- (ycor (seg-end s))
                 (ycor (seg-start s)))))
          (sqrt (+ (square dx)
                   (square dy))))))



;; but all are defined by cons, car cdr... primitive procedures??
;; So... I say: "Hey, is primitive! Is magic". NO!!
;; We can build that procedures that way:


(defun crazy-cons (a b)
  (lambda (pick)
    (cond ((= pick 1) a)
          ((= pick 2) b))))

(defun crazy-car (λ) (funcall λ 1))

(defun crazy-cdr (λ) (funcall λ 2))