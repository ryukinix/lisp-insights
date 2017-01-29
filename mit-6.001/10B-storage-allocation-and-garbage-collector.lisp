;; Common Lisp Script
;; Manoel Vilela


;; Storage Allocation and Garbage Collector
;; Representing Memory List Structure

;; ... again register machines. This is a lot of mess.

;; Godel says that any system information can be represented by numbers in that way:
;; (cons x y) => 2^x * 3^y, so (cons 1 1) => 2 * 3 = 6

(defmacro define-assembly (name &rest instructions)
  `(defparameter ,name (quote ,instructions)))


(define-assembly freelist-allocation
  (assign a (cons (fetch b)
                  (fetch c)))
  (assign a (fetch free))
  (assign free (vector-ref (fetch the-cdrs)
                           (fetch free)))
  (perform (vector-set! (fetch the-cars)
                        (fetch a)
                        (fetch b)))
  (perform (vector-set! (fetch the-cdrs)
                        (fetch a)
                        (fetch c))))


;; garbage collector
(define-assembly gc-code
  gc
  (assign thing (fetch root))
  (assign continue)
  mark
  (branch (not-pair? (fetch thing))
          done)
  pair
  (assign mark-flag
          (vector-ref (fetch the-marks)
                      (fetch thing)))
  (branch (= (fetch mark-flag) 1)
          done)
  (perform (vector-set! (fetch the-marks)
                        (fetch thing)))
  mcar
  (push thing)
  (push continue)
  (assign continue mcdr)
  (assign thing (vector-ref (fetch the-cars)
                            (fetch thing)))
  (goto mark)
  mcdr
  (pop continue)
  (pop thing)
  (assign thing
          (vector-ref (fetch the-cdrs)
                      (fetch thing)))
  (goto mark)
  done
  (goto (fetch continue)))

(define-assembly auxiliary-gc
  (assign free '())
  (assign scan (1- (fetch memtop)))
  slp
  (branch (negative? (fetch scan))
          end)
  (assign mark-flag
          (vector-ref (fetch the-marks)
                      (fetch scan)))
  (branch (= (fetch mark-flag)
             1)
          unmk)
  (perform (vector-set! (fetch the-cdrs)
                        (fetch scan)
                        (fetch free)))
  (assign free (fetch scan))
  (assign scan (1- (fetch scan)))
  (goto slp)
  unmk
  (perform (vector-set! (fetch the-marks)
                        (fetch scan)
                        0))
  (assign scan (1- (fetch scan)))
  (goto slp)
  end)

;; cited fastest garbage collector algorithm:
;; Minsky-Feinchel-Yochelson Garbage Collector Algorithm, 61'

;; The lecture finishes introducing the halting problem
;; and problems not computables.

(defun inf ()
  (lambda () (funcall (lambda (x) (funcall x x))
                 (lambda (x) (funcall x x)))))

(defun diag1 (p)
  (if (safe? p p)
      (inf)
      3))

(diag1 diag1) ;; safe?

;; diag here comes to the Diagonal Argument of Cantor
;; proving real numbers are not countable by showing
;; that the numbers between a segment line is bigger than
;; all set of natural numbers -- Sussman explanation
(defun diag2 (p)
  (if (safe? p p)
      (other-than (p p))
      'false))


(defun other-than (p)
  (if (eq p 'x)
      'x
      p))

;; IS NOT POSSIBLE TO TELL IF A FUNCTION WILL GET A INFINITE LOOP
;; UNTIL YOU RUN IT.
