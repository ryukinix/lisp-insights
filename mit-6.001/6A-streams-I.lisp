;; Common Lisp Script
;; Manoel Vilela


;; THIS LECTURE IS ABOUT STREAMS AND GENERATORS


;; previous concepts defined:
;; * STATE
;; * ASSIGNMENT
;; * CHANGE (VARIABLE)
;; * TIME
;; * IDENTITY (ABOUT CAR/CDRS)
;; * OBJECT
;; * SHARING (STATE)


;; without using streams and high abstract procedures, solve that two problems:
;; sum-odd-squares on tree and sum the odd fib numbers from a interval [n, m]


(defun leaf-node? (tree)
  (atom tree))

(defun square (n)
  (* n n))

(defun left-branch (tree)
  (car tree))

(defun right-branch (tree)
  (cdr tree))

(defun sum-odd-squares (tree)
  (if (leaf-node? tree)
      (if (and (numberp tree) (oddp tree))
          (square tree)
          0)
      (+ (sum-odd-squares
          (left-branch tree))
         (sum-odd-squares
          (right-branch tree)))))

;; tail cail optimized, from O(2^n) => O(n)
(defun fib (n)
  (labels ((tail-call-fib (n acc1 acc2)
             (if (= n 0)
                 acc1
                 (tail-call-fib (1- n) acc2 (+ acc1 acc2)))))
    (tail-call-fib n 0 1)))

(defun odd-fibs (n)
    (labels ((next (k)
               (if (> k n)
                   nil
                   (let ((f (fib k)))
                     (if (oddp f)
                         (cons f (next (1+ k)))
                         (next (1+ k)))))))
      (next 1)))

;; However, we can imagine that problems in a signal processing evaluation, like that

;; sum-odd-squares
;; enumerates-leaves => filter-odd => map-square => (acc '+ '0)

;; odd-fibs
;; enum-interval => map-fib => filter-odd => (acc 'cons '())

;; This is like streams, so we will define data abstraction for streams
;; primitives
;; (cons-stream x y)
;; (head s)
;; (tail s)
;; (the-empty-stream)

;; for any x and y
;; (head (cons-stream x y)) => x
;; (tail (cons-stream x y)) => y

;; this seems very likely cons, car and cdr, right?
;; but not! streams are lazy evaluated


(defun delay (x)
  (lambda () x))

(defun force (x)
  (let* ((exp (funcall x))
         (func (car exp))
         (args (cdr exp)))
    (apply func args)))

;; i tried define this as macro to avoid the quotation for y,
;; but just doesn't works T_T
;; old version ::
;; (defmacro cons-stream (x delayed)
;;   `(cons ,x (delay (list ,@delayed))))
;; the code fails on enumerate-tree about
;; (cons tree the-empty-stream) ;; => (cons tree (list nil))
(defun cons-stream (x y)
  (cons x (delay y)))

(defun head (x)
  (car x))

(defun tail (x)
  (force (cdr x)))

(defparameter the-empty-stream '())

(defun empty-stream? (x)
  (null x))

(defun map-stream (proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream
       (funcall proc (head s))
       `(map-stream ,proc ,(tail s)))))

(defun filter (pred s)
  (cond ((empty-stream? s) the-empty-stream)
        ((funcall pred (head s))
         (cons-stream (head s)
                      `(filter ,pred
                               ,(tail s))))
        (t (filter pred (tail s)))))

(defun accumulate (combiner init-val s)
  (if (empty-stream? s)
      init-val
      (funcall combiner (head s)
               (accumulate combiner
                           init-val
                           (tail s)))))

(defun append-streams (s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   `(append-streams ,(tail s1)
                                    ,s2))))

(defun enumerate-tree (tree)
  (if (leaf-node? tree)
      (cons-stream tree
                   the-empty-stream)
      (append-streams
       (enumerate-tree
        (left-branch tree))
       (enumerate-tree
        (right-branch tree)))))

(defun enum-interval (low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low `(enum-interval ,(1+ low) ,high))))


(defun sum-odd-squares-stream (tree)
  (accumulate #'+ 0
              (mapcar #'square
                   (filter #'oddp
                           (enumerate-tree tree)))))


(defun odd-fibs-stream (n)
  (accumulate #'cons '()
              (filter #'oddp
                      (mapcar #'fib (enum-interval 1 n)))))


(defun flatten (st-of-st)
  (accumulate #'append-streams
              the-empty-stream
              st-of-st))

(defun flat-map (f s)
  (flatten (map-stream f s)))


(defun prime? (n)
  (let ((divisors (loop for x from 2 to (round (sqrt n)) collect x)))
    (loop for div in divisors never (eq (mod n div) 0))))

(defun prime-sum-pairs (n)
  (map-stream #'(lambda (p)
                  (list (car p)
                        (cadr p)
                        (+ (car p) (cadr p))))
              (filter
               (lambda (p)
                 (prime? (+ (car p) (cadr p))))
               (flat-map #'(lambda (i)
                             (map-stream (lambda (j) (list i j))
                                         (enum-interval 1 (1- i))))
                         (enum-interval 1 n)))))

(defun range (a b)
  (enum-interval a b))

(defun eval-stream (s)
  (if (empty-stream? s)
      nil
      (cons (head s) (eval-stream (tail s)))))

(eval-stream (range 1 10))

(prime? 13)
(head (tail (filter #'prime? (enum-interval 10000 100000000000000))))
