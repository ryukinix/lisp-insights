;; Common Lisp Script
;; Manoel Vilela

;; The Hardware Machine Description in Lisp

;; a simple macro to that file compiles.
(defmacro define-machine (machine &rest body)
  `(defparameter ,machine (quote ,body)))

(defun gcd. (a b)
  (if (= b 0)
      A
      (gcd. b (mod a b))))

;; A machine description for the GCD procedure/machine
(define-machine gcd-machine
  (registers a b t)
  (controller
   main (assign a (read))
        (assign b (read))
   loop (branch (zerop (fetch b))
                done)
        (assign t (mod (fetch a) (fetch b)))
        (assign a (fetch b))
        (assign b (fetch t))
        (goto loop)
   done (perform (print (fetch a)))
        (goto main)))


(defun fact (n)
  (if (= n 1)
      1
      (* n (fact (1- n)))))


(fact 10000)

;; fact is a problem, because before build this machine, we need fact exists
;; and remember the last operations to reduce the overall expression
;; only in the end => n! = n*(n -1)...1
;; for that, will put each `n` on stack before the recursive call


(define-machine fact-machine
  (registers n)
  (assign continue done)
  (controller
   loop (branch (= 1 (fetch n)) base)
        (save continue)
        (save n)
        (assign n (-1 (fetch n)))
        (assign continue aft)
        (goto loop)
    aft (restore n)
        (restore continue)
        (assign val (* (fetch n) (fetch val)))
        (goto (fetch continue))
   base (assign val (fetch n))
        (goto (fetch continue))
   done))



(defun fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

;; this is not lisp code, ok?
;; is a embed language for machine code made in lisp
;; to be interpreted in lisp, but is not lisp!!!!!!!!
(define-machine fib-machine
  (registers n)
  (controller
     (assign continue fib-done)
   fib-loop ; n contains arg, continue as recipient
     (branch (< (fetch n) 2) immediate-ans)
     (save continue)
     (assign continue after-fibs-n-1)
     (save n)
     (assign n (- (fetch n) 1))
     (goto fib-loop)
   after-fib-n-1 ; after get the first fib
     (restore n)
     (restore continue) ; useless?
     (assign n (- (fetch n) 2))
     (save continue) ; useless?
     (assign continue after-fib-n-2)
     (save val)
     (goto fib-loop)
   after-fib-n-2
     (assign n (fetch val)) ;; fib(n - 2)
     (restore val)
     (restore continue)
     (assign val
             (+ (fetch val)
                (fetch n))) ;; fib(n -1) + fib(n - 2)
     (goto fetch continue)
   immediate-ans
     (assign val (fetch n))
     (goto (fetch continue))
   fib-done))


;; Machine primitives: DATAPATH + CONTROLLER + MEMORY


;; actually, this really seems a low-level description for
;; a machine, Lisp Assembly? HAHA... But I don't get the whole
;; picture of this lecture. Maybe I need rewatch this in someday
