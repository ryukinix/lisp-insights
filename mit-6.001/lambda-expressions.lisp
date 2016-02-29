;; Common Lisp Script
;; Manoel Vilela


;; function sigma-sum
;; func must be a lambda(x) expression
(defun sigma-sum (func a b &optional (next #'1+))
  (if (> a b)
      0
      (+ (funcall func a)
         (sigma-sum func (funcall next a) b))))

;; I would like to avoid the use of 'funcall' for calling
;; lambda expressions passed to sigma-sum, is ugly.

(defun sum-square (a b)
  (sigma-sum #'(lambda(x) (* x x)) a b))

(defun sum-int (a b)
  (sigma-sum #'(lambda(x) x) a b))

(defun sum-pi (a b)
  (sigma-sum #'(lambda (x) (/ 1 (* x (+ 1 2)))) 
             a b
             #'(lambda (x) (+ x 4))))

(sum-square 1 10)
(sum-int 1 10)
(sigma-sum #'(lambda(x) (/ 1 x)) 1 10 #'(lambda (x) (+ 4 x)))
(sigma-sum #'(lambda(x) (- x)) 1 10)