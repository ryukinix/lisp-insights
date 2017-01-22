;; Common Lisp Script
;; Manoel Vilela

#|

  -- * A META-CIRCULAR COMPILER OF MINIMAL LISP * --

  Every function symbol with prefixed '+ is used to not conflict with the namespace of reserved special forms
  of the current Lisp compiler.
|#

;; assumes lambda exp => (closure ((vars) body) env)

(defun primitive? (_)
  (or _ t))

(defun apply-primeop (proc args)
  (+apply proc args))


(defun pair-up (vars vals)
  (loop for x in vars
        for y in vals
        collect (cons x y)))

(defun assq (sym alist)
  (cond ((eq alist '()) nil)
        ((eq sym (caar alist))
         (car alist))
        (t (assq sym (cdr alist)))))

(defun lookup (sym env)
  (cond ((eq env '()) (error 'UBV)) ;; UNBOUND VARIABLE
        (t (funcall (lambda (vcell)
                      (cond ((eq vcell '())
                             (lookup sym (cdr env)))
                            (t (cdr vcell))))
                    (assq sym (car env))))))

(defun bind (vars vals env)
  (cons (pair-up vars vals) env))

(defun +eval (exp env)
  (cond ((numberp exp) exp)
        ((symbolp exp) (lookup exp env))
        ((eq (car exp) 'quote) (cadr exp))
        ((eq (car exp) 'lambda)
         (list 'closure (cdr exp) env))
        ((eq (car exp) 'cond)
         (evcond (cdr exp) env))
        (t (+apply (+eval (car exp) env)
                  (+eval (cdr exp) env)))))


(defun +apply (proc args)
  (cond ((primitive? proc)
         (apply-primeop proc args))
        ((eq (car proc) 'closure)
         (+eval (cadadr proc)
               (bind (caadr proc)
                     args
                     (caddr proc))))
        (t 'error)))

(defun false? (x)
  (eq x nil))

(defun evlist (l env)
  (cond ((eq l '()) '())
        (t (cons
            (+eval (car l) env)
            (evlist (cdr l) env)))))

(defun evcond (clauses env)
  (cond ((eq clauses '()) '())
        ((eq (caar clauses) t)
         (+eval (cadar clauses) env))
        ((false? (+eval (caar clauses) env))
         (evcond (cdr clauses) env))
        (t (+eval (cadar clauses) env))))


#| Evaluating expression with this compiler using substitution model

=> (eval '(((lambda (x) (lambda (y) (+ x y))) 3) 4) <e0>)
=> (apply (eval '((lambda (x) (lambda (y) (+x y))) 3) <e0>)
       (evlist '(4) <e0>))
=> (apply (eval '((lambda (x) (lambda (y) (+ x y))) 3) <e0>)
          (cons (eval '4 <e0)
                (evlist '() <e0>)))
=> (apply (eval '((lambda (x) (lambda (y) (+ x y))) 3) <e0>)
          (cons 4 '()))
=> (apply (eval '((lambda (x) (lambda (y) (+ x y))) 3) <e0>)
          '(4))
=> (apply (apply (eval '(lambda (x) (lambda (+ x y))) <e0>)
                 '(3))
          '(4))

=> (apply (apply '(closure ((x) (lambda (y) (+ x y))) <e0>)
                  '(3))
          '(4))
=> (apply (eval '(lambda (y) (+ x y)) <e1>)
          '(4))
=> (apply '(closure ((y) (+ x y)) <e1>)
          '(4))
=> (eval (+ x y) <e2>)
=> (apply (eval '+ <e2>)
          (evlist '(x y) <e2>))
=> (apply '+ '(3 4))
7

The eval/apply dancing.

EVAL → PROC, ARGS → APPLY
 ↓                    ↓
 ====> EXP, ENV <======


|#


;; fixed points :: (Y F) = (Y (Y F))


(defun Y (f)
  (funcall (lambda (x) (funcall f x x))
           (lambda (x) (funcall f x x))))

;; (y #'print) bug!, y combinators!

#|
Y = (lambda (f)
      ((lambda (x) (f (x x)))
       (lambda (x) (f (x x)))))

(Y F) = ((lambda (x) (F (x x)))
         (lambda (x) (F (x x))))
      = (F ((lambda (x) (F (x x)))
            (lambda (x) (F (x x)))))
(Y F) = (F (Y F))

|#
