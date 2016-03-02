;; Common Lisp Script
;; Manoel Vilela

(flet ((foo (when funcall) 
        (when (< 3 when) 
          (funcall funcall)) when)) 
      (loop :named funcall :for funcall :from 1 :collect 
            (foo funcall (lambda () (loop-finish)))))