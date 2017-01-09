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

;; square-root black box
;; functional by function composition
(defun square-root (x &key (error-limit *error-limit*))
  (labels ((improve (guess)
             (average guess (/ x guess)))
           (good-enough? (guess)
             (< (abs (- (square guess) x))
                error-limit))
           (try (guess)
             (if (good-enough? guess)
                 guess
                 (try (improve guess)))))

  (float (try 1))))


;; eval-test black-box
;; functional
(defun eval-test (test fn limit)
  (labels ((call-test (test)
             (let ((output (eval test)))
                   (format t "~s -> ~f ~%" test output)
                   output)))
    (let ((x (cadr test)))
        (if (< (abs (- (call-test test) (funcall fn x)))
              limit)
           :nice
           :fail))))

;; non-functional use variable global *error-limit* and *tests*
(defun run-tests ()
  (format t "Running tests with limit ~f ~%" *error-limit*)
  (let* ((results (loop for x in *tests* collect (eval-test x #'sqrt *error-limit*)))
         (total (length results))
         (pass (count :nice results)))
      (format t "Tests avalied [pass/total]: ~d/~d ~%" pass total)))


(run-tests)
