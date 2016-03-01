;; Common Lisp Script
;; Manoel Vilela

(defun compose (&rest funs)
  (let ((funs (reverse funs)))
    (lambda (&rest args)
      (loop with result = args
            for fun in funs
            do (setf result (list (apply fun result)))
            finally (return (car result))))))

(funcall (compose #'abs #'-) 1 2) #| ==> 1 |#