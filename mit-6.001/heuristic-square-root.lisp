;; Common Lisp Script
;; Manoel Vilela

#| A heuristic method to calculate the 
   square root of a number x based on the 
   Hero of Alexandria's alogrithm.
 |#

 
(defparameter *error-limit* 0.001)

(defparameter *tests* '((square-root 2)
                        (square-root 12)
                        (square-root 25)
                        (square-root 144)
                        (square-root 165465)
                        ))

(defun square (x)
  (* x x))


(defun average (x y) 
  (/ (+ x y) 2))


(defun improve (guess x)
  (average guess (/ x guess)))


(defun good-enough? (guess x)
  (< (abs (- (square guess) x))
     *error-limit*))


(defun try (guess x)
  (if (good-enough? guess x)
      guess
      (try (improve guess x) x)))


(defun square-root (x) (float (try 1 x)))


(defun call-test (test)
  (let ((output (eval test)))
       (format t "~s -> ~f ~%" test output)
       output))


(defun eval-test (test fn)
   (let ((x (cadr test)))
       (if (< (abs (- (call-test test) (funcall fn x)))
              *error-limit*)
           :nice
           :fail)))

(defun run-tests (tests)
  (format t "Running tests with limit ~f ~%" *error-limit*)
  (let* ((results (loop for x in *tests* collect (eval-test x #'sqrt)))
         (total (length results))
         (pass (count :nice results)))
      (format t "Tests avalied [pass/total]: ~d/~d ~%" pass total)))


(run-tests *tests*)