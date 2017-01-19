;; Common Lisp Script
;; Manoel Vilela

;; WARNING: THIS CODE WAS COPIED DIRECTLY FROM SICP AND I DON'T TESTED IT

;; In the original SICP lectures instead the symbol '=>' is used the colon ':'
;; however that char is reserved at least on SBCL and a error is dispatched when
;; this is evaluated
(defparameter deriv-rules '(;; constant
                            ((dd (?c c) (? x)) 0)
                            ;; same var
                            ((dd (?v x) (? x)) 1)
                            ;; another var
                            ((dd (?v y) (? x)) 0)
                            ;; sum rule
                            ((dd (+ (? x1) (? x2)) (? x))
                             (+ (dd (=> x1) (=> x))
                              (dd (=> x2) (=> x))))
                            ;; multiplication rule
                            ((dd (* (? x1) (? x2)) (? x))
                             (+ (* (=> x1) (dd (=> x2) (=> x)))
                                (* (=> x2) (dd (=> x1) (=> x)))))
                            ;; exponentiation rule
                            ((dd (** (? fx) (? n)) (? x))
                             (* (* (=> n)
                                   (** (=> fx) (1- (=> n))))
                              (dd (=> fx) (=> x))))))

(defparameter algebra-rules '(;; if the operands are constants, evaluate it
                              ;; (+ 1 2) => 3
                              (((? op) (?c e1) (?c e2))
                               (=> (op e1 e2)))

                              ;; if second operand is constant and first not, swap
                              ;; (+ x 1) => (+ 1 x)
                              (((? op) (? e1) (?c e2))
                               ((=> op) (=> e2) (=> e1) ))

                              ;; if something sums 0, return that thing
                              ;; (+ 0 x) => x
                              ((+ 0 (? e)) (=> e))

                              ;; if something multiplies 1, just return that
                              ;; (* 1 x) => x
                              ((* 1 (? e)) (=> e))

                              ;; anything multiplied by zero is zero
                              ;; (* 0 x) => 0
                              ((* 0 (? e)) 0)

                              ;; constant multiplication
                              ;; (* 2 (* 2 x)) => (* 4 x)
                              ((* (?c e1) (* (?c e2) (? e3)))
                               (* (=> (* e1 e2)) (=> e3)))

                              ;; aesthetic multiplication simplification
                              ;; (* x (* 5 y)) => (* 5 (* x y))
                              ((* (? e1 (* (?c e2) (? e3))))
                               (* (=> e2) (* (=> e1) (=> e3))))

                              ;; sum evaluation if is possible
                              ;; (+ 5 (+ 3 x)) => (+ 8 x)
                              ((+ (?c e1) (+ (?c e2) (? e3)))
                               (+ (=> (+ e1 e2)) (=> e3)))

                              ;; aesthetic sum simplification
                              ;; (+ x (+ 2 y)) => (+ 2 (+ x y))
                              (((+ (? e1) (+ (?c e2) (? e3)))
                                (+ (=> e2) (+ (=> e1) (=> e3)))))

                              ;; commutative multiplication property
                              ;; (* (* x y) z) => (* x (* y z))
                              ((* (* (? e1) (? e2)) (? e3))
                               (* (=> e1) (* (=> e2) (=> e3))))

                              ;; commutative sum property
                              ;; (+ (+ x y) z) => (+ x (+ y z))
                              ((+ (+ (? e1) (? e2)) (? e3))
                               (+ (=> e1) (+ (=> e2) (=> e3))))

                              ;; algebraic sum
                              ;; (+ (* 2 x) (* 4 x)) => (* 6 x)
                              ((+ (* (?c a) (? x)) (* (?c b) (? x)))
                               (* (=> (+ a b)) (=> x)))

                              ;; distribution rule of product over sum
                              ((* (? c) (+ (? d) (? e)))
                               (+ (* (=> c) (=> d))
                                  (* (=> c) (=> e))))))

;; matcher machine
;;                pattern
;;                   ↓
;;              +---------+
;;              |         |
;; expression → | MATCHER | → dict
;;              |         |
;;              +---------+
;;                   ↑
;;                  dict
;;

(defparameter empty-dictionary '())

(defun extend-dictionary (pat dat dict)
  (let* ((name (variable-name pat))
         (v (assq name dict)))
    (cond ((null v)
           (cons (list name dat) dict))
          ((eq (cadr v) dat) dict)
          (t 'failed))))

(defun lookup (var dict)
  (let ((v (assq var dict)))
    (if (null v)
        var
        (cadr v))))

(defun match (pat exp dict)
  (cond ((eq dict 'failed) 'failed)
        ((atom pat)
         (if (atom exp)
             (if (eq pat exp)
                 dict
                 'failed)
              'failed))
        ((arbitrary-constants? pat)
         (if (constant? exp)
             (extend-dict pat exp dict)
             'failed))
        ((arbitrary-variable? pat)
         (if (variable? exp)
             (extend-dict pat exp dict)
             'failed))
        ((arbirary-expression? pat)
         (extend-dict pat exp dict))
        ((atom exp) 'failed)
        (t (match (cdr pat)
                  (cdr exp)
                  (match (car pat)
                         (car exp)
                         dict)))))

(defun instantiate (skeleton dict)
  (defun loop-inst (s)
    (cond ((atom s) s)
          ((skeleton-evaluation? s)
           (evaluate (eval-exp s) dict))
          (t (cons (loop-inst (car s))
                   (loop-inst (cdr s))))))
  (loop-inst skeleton))

(defun evaluate (form dict)
  (if (atom form)
      (lookup form dict)
      (apply
        (eval (lookup (car form) dict)
             user-initial-environment)
        (mapcar (lambda (v)
                  (lookup v dict))
                (cdr form)))))


(defun simplifier (the-rules)
  (defun try-rules (exp)
    (defun scan (rules)
      (if (null rules)
          exp
          (let ((dict (match (pattern (car rules))
                              exp
                        (empty-dictionary))))
            (if (eq dict 'failed)
                (scan (cdr rules))
                (simplify-exp
                  (instantiate
                   (skeleton (car rules))
                   dict))))))
    (scan the-rules))
  (defun simplify-parts (exp)
    (if (null exp)
        '()
        (cons (simplify-exp (car exp))
              (simplify-parts (cdr exp)))))
  (defun simplify-exp (exp)
    (try-rules (if (compound? exp)
                   (simplify-parts exp)
                   exp)))

  simplify-exp)


(setf (symbol-function 'dsimp) (simplifier deriv-rules))
