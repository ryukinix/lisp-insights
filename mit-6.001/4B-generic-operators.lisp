;; Common Lisp Script
;; Manoel Vilela

;; Arithmetic operations on
;; complex numbers


;; ** COMPLEX NUMBERS PROPERTIES **
;; Re(Z1 + Z2) = Re(Z1) + Re(Z2)
;; Im(Z1 + Z2) = Im(Z1) + Im(Z2)
;; Mag(Z1 * Z2) = Mag(Z1) * Mag(Z2)
;; Angle(Z1 * Z2) = Angle(Z1) + Angle(Z2)

;; ** SELECTORS **
;; REAL-PART -> Z
;; IMG-PART -> Z
;; MAGNITUDE -> Z
;; ANGLE -> Z

;; ** CONSTRUCTORS **
;; MAKE-RECTANGULAR -> X -> Y
;; MAKE-POLAR -> R -> A

;; SOLUTION FOR MULTIPLE REPRESENTATIONS
;; TYPED DATA + GENERIC OPERATORS

;; constructor
(defun attach-type (type contents)
  (cons type contents))

;; selectors
(defun complex-type (datum)
  (car datum))

(defun contents (datum)
  (cdr datum))

;; type predicates

(defun rectangular? (z)
  (eq (complex-type z) 'rectangular))

(defun polar? (z)
    (eq (complex-type z) 'polar))

;; GEORGE IMPLEMENTATION
;; Representing complex numbers as
;; pairs REAL-PART, IMAGINARY-PART

(defun make-rectangular (x y)
  (attach-type 'rectangular (cons x y)))

(defun real-part-rectangular (z)
  (car z))

(defun imag-part-rectangular (z)
  (cdr z))

(defun magnitude-rectangular  (z)
  (sqrt (+ (square (car z))
           (square (cdr z)))))

(defun angle-rectangular (z)
  (atan (/ (cdr z) (car z))))

;; MARTHA IMPLEMENTATION
;; Representing complex numbers as
;; pairs MAGNITUDE, ANGLE


(defun make-polar (r a)
  (attach-type 'polar (cons r a)))

(defun magnitude-polar (z)
  (car z))

(defun angle-polar (z)
  (cdr z))

(defun real-part-polar (z)
  (* (car z) (cos (cdr z))))

(defun imag-part-polar (z)
  (* (car z) (sin (cdr z))))

;; GENERIC SELECTORS FOR COMPLEX NUMBERS


(defun real-part (z)
  (cond ((rectangular? z)
         (real-part-rectangular
          (contents z)))
        ((polar? z)
         (real-part-polar
          (contents z)))))

(defun imag-part (z)
  (cond ((rectangular? z)
         (imag-part-rectangular
          (contents z)))
        ((polar? z)
         (imag-part-polar)))
  (contents z))

(defun magnitude (z)
  (cond ((rectangular? z)
         (magnitude-rectangular
          (contents z)))
        ((polar? z)
         (magnitude-polar
          (contents z)))))

(defun angle (z)
  (cond ((rectangular? z)
         (angle-rectangular
          (contents z)))
        ((polar? z)
         (angle-polar
          (contents z)))))

(defun +c (z1 z2)
  (make-rectangular
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(defun -c (z1 z2)
  (make-rectangular
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(defun *c (z1 z2)
  (make-polar
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(defun /c (z1 z2)
  (make-polar
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))

;; an alternative way to represent this, avoid the MANAGER is using a table
;; for relate each type and the correct procedure. In the lecture is build on top using this technique for rational, polynomial, complex and ordinary numbers the generic operators add, sub, mul and div.
