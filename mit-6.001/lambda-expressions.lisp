;; Common Lisp Script
;; Manoel Vilela


;; higher-order function sigma-sum
;; func must be a lambda(x) expression
(defun sigma-sum (func a b &key (next #'1+))
  (if (> a b)
      0
      (+ (funcall func a)
         (sigma-sum func (funcall next a) b))))

(defun square (x)
  (* x x))

(sigma-sum #'(lambda(x) (/ 1 x)) 1 10 :next #'(lambda (x) (+ 4 x)))
(sigma-sum #'(lambda(x) (- x)) 1 10)