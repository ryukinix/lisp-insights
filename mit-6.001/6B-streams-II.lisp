;; Common Lisp Script
;; Manoel Vilela

(load "6A-streams-I.lisp")


;; i just watched this lecture, some behaviors of stream don't works well here
;; i can't define things like (define ones (cons-stream 1 ones))
;; the primitive for streams on previous lecture is messed up
;; the problem is store a operand of procedure without evaluated it before
;; as common lisp eval all your operands before doing a funcall,
;; delay procedure doesn't works well.
