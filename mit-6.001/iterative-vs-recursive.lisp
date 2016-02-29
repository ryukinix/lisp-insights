;; Common Lisp Script
;; Manoel Vilela

(defun print-eval (expression)
  (eval (print expression)))

;; linear iterative solution
;; time=O(x); space=O(1) 
(defun sum-i (x y)
  (if (= x 0)
      y
      (print-eval `(sum-i (1- ,x) (1+ ,y)))))


;; linear recursive solution
;; time=O(x); space=O(x)
;; but lisp have tail call optimization
;; so we receive the same of sum-i on printing
(defun sum-r (x y)
  (if (= x 0)
      y
      (print-eval `(1+ (sum-r (1- ,x) ,y)))))


(defun fib-r (n)
  (if (< n 2)
      n
      (print-eval `(+ (fib-r (- ,n 1))
                      (fib-r (- ,n 2))))))

(print (sum-i 10 20))
(print (sum-r 10 20))
(print (fib-r 10))