;; Common Lisp Script
;; Manoel Vilela

;; simple lambda usage with mapcar
(mapcar (lambda (x) (/ x 2)) '(1 2 3 4)) ;; => (1/2 1 3/2 2)

;; is equivalent
(defun half (x)
  (/ x 2))

(mapcar #'half '(1 2 3 4))

;; lambda' are a macro, so your operands are not evaluated first
