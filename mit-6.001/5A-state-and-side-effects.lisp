;; Common Lisp Script
;; Manoel Vilela

;; mathematical truths using functional programming
;; don't change state, the same funcall for same params
;; the result is the same

(defun fact (n)
  (if (<= n 1)
      1
      (* n (fact (1- n)))))

;; imperative way
(defun fact (n)
  (let ((i 1)
        (m 1))
    (labels ((iter ()
               (if (> i n)
                   m
                   (progn (setq m (* i m))
                          (setq i (1+ i))
                          (iter)))))
      (iter))))

(fact 10)

;; using mutation of state
;; lambda used as closure
(defun make-counter (n)
  (lambda ()
    (setq n (1+ n))
    n))

(setf (symbol-function 'c1) (make-counter 1))
(setf (symbol-function 'c2) (make-counter 10))
(c1) ;; independents states for n
(c2) ;; makes change on our environment

;; examples with free variable

(defun free-variable-x ()
  (lambda (x) (lambda (y) (* x y))))
;;                   ↑ x is a free-variable get in the environment at that moment
