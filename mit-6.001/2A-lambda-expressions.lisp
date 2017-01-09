;; Common Lisp Script
;; Manoel Vilela

;; function Σ
;; func must be a lambda(x) expression
(defun Σ (λ a b &optional (next #'1+))
  (if (> a b)
      0
      (+ (funcall λ a)
         (Σ λ (funcall next a) b))))

;; I would like to avoid the use of 'funcall' for calling
;; lambda expressions passed to Σ, is ugly.

(defun sum-square (a b)
  (Σ #'(lambda(x) (* x x)) a b))

(defun sum-int (a b)
  (Σ #'(lambda(x) x) a b))

(defun sum-pi (a b)
  (Σ #'(lambda (x) (/ 1 (* x (+ 1 2)))) 
             a b
             #'(lambda (x) (+ x 4))))

(sum-square 1 10)
(sum-int 1 10)
(Σ #'(lambda(x) (/ 1 x)) 1 10 #'(lambda (x) (+ 4 x)))
(Σ #'(lambda(x) (- x)) 1 10)