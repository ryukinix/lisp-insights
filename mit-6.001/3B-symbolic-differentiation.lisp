;; Common Lisp Script
;; Manoel Vilela


(defparameter *dx* 0.0001)

(defun deriv-numerical (f)
  (lambda (x)
    (/ (- (funcall f (+ x *dx*))
          (funcall f x))
       *dx*)))

;; x^3 => 3x^2
;; 2^3 => 8
;; 3 * (2^2) => 12
(funcall (deriv-numerical (lambda (x) (* x x x))) 2) ;; => 11.987

(defun constant? (exp var)
  (and (atom exp)
       (not (eq exp var))))

(defun same-var? (exp var)
  (and (atom exp)
       (eq exp var)))

(defun sum? (exp)
  (and (not (atom exp))
       (eq (car exp) '+)))

(defun power? (exp)
  (and (not (atom exp))
       (eq (car exp) '^)))

(defun make-sum (a1 a2)
  (list '+ a1 a2))

(defun make-product (m1 m2)
  (list '* m1 m2))

(defun power-rule (exp)
  (let ((base (cadr exp))
        (pow (caddr exp)))
    (list '* pow `(^ ,base ,(1- pow)))))
(deriv '(^ x 3) 'x)

(setf (symbol-function 'a1) #'cadr)
(setf (symbol-function 'a2) #'caddr)
(setf (symbol-function 'm1) #'cadr)
(setf (symbol-function 'm2) #'caddr)
(setf (symbol-function '^) #'expt) ;; power symbol function

(defun product? (exp)
  (and (not (atom exp))
       (eq (car exp) '*)))

(defun deriv (exp var)
  (cond ((constant? exp var) 0)
        ((same-var? exp var) 1)
        ((sum? exp)
         (make-sum (deriv (a1 exp) var)
                   (deriv (a2 exp) var)))
        ((product? exp)
         (make-sum
          (make-product (m1 exp)
                        (deriv (m2 exp) var))
          (make-product (m2 exp)
                        (deriv (m1 exp) var))))
        ((power? exp) (power-rule exp))))

(defparameter foo '(+ (* a ( * x x))
                    (+ (* b x) c)))
;; second version of representation
;; simplifying algebraic expressions

(defun make-sum (a1 a2)
  (cond ((and (numberp a1)
              (numberp a2))
         (+ a1 a2))
        ((and (numberp a1) (= a1 0))
         a2)
        ((and (numberp a2) (= a2 0))
         a1)
        (t (list '+ a1 a2))))

(defun make-product (m1 m2)
  (cond ((and (numberp m1)
              (numberp m2))
         (+ m1 m2))
        ((or (and (numberp m1) (= m1 0))
             (and (numberp m2) (= m2 0)))
         0)
        ((and (numberp m1) (= m1 1))
         m2)
        ((and (numberp m2) (= m2 1))
         m1)
        (t (list '* m1 m2))))

(deriv foo 'x)
