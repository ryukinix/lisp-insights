;; Common Lisp Script
;; Manoel Vilela

(load "7A-metacircular-evaluator-I.lisp")

#|
 == META-CIRCULAR EVALUATOR PART ii ==

A weird interpreter of Lisp written in Lisp to run it in Lisp. Why so much Lisp? I even know what I writing.
Well, let me press the play button for the lecture 7B.

|#


(defun take-many-arguments (x &rest y)
  (mapcar #'(lambda (u) (* u x))
          y))

(take-many-arguments 5 1 2 3 4 5)


;; some strange modification was done here to support
;; function lambdas with variadic params
(defun pair-up (vars vals)
  (cond ((eq vars '())
         (cond ((eq vals '()) '())
               (t (error 'TMA))))
        ((symbolp vars)
         (cons (cons vars vals) '()))
        ((eq vals '())
         (error 'TFA))
        (t (cons (cons (car vars)
                       (car vals))
                 (pair-up (cdr vars)
                          (cdr vals))))))

(defun sum (term a next b)
  (if (> a b)
      0
      (+ (funcall term a)
         (sum term
              (funcall next a)
              next
              b))))

(defun product (term a next b)
  (if (> a b)
      0
      (* (funcall term a)
         (product term
              (funcall next a)
              next
              b))))

(defun sum-powers (a b n)
  (sum (lambda (x) (expt x n))
       a
       #'1+
       b))

(defun product-powers (a b n)
  (product (lambda (x) (expt x n))
           a
           #'1+
           b))

;; this is not so nice, we have code replication. Hour to do some abstraction here

;; => POC

(defun nth-power (x)
  (expt x n)) ;; a problem here, n is isolated into this environment, n => free variable
;; but how can nth-power know if n is a bound variable when is called?

;; the name of that problem is dynamic binding.

(defun sum-powers (a b n)
  (sum #'nth-power a #'1+ b))
;; nth-power, with dynamical binding, we try look for the value of n into the environment of caller.
;; which in that case is sum-of-powers, so will n from nth-power bind to n to sum-powers function body.

;; similarly to product we have

(defun product-powers (a b n)
  (product #'nth-power a #'1+ b))

;; for that works we need change the way of +eval from the last lecture is implemented, here is the new +eval:

;; we'll kill the lexical scope and statical binding for that, so closures are dead.

(defun +eval (exp env)
  (cond ((numberp exp) exp)
        ((symbolp exp) (lookup exp env))
        ((eq (car exp) 'quote) (cadr exp))
        ((eq (car exp) 'lambda) exp) ;; changes here, just return the lambda exp instead closure/env
        ((eq (car exp) 'cond)
         (evcond (cdr exp) env))
        (t (+apply (+eval (car exp) env)
                   (evlist (cdr exp) env) ;; also changes here
                   env))))

;; apply needs be changed too, and gets more complicated

(defun +apply (proc args env) ;; now apply needs env as parameter!
  (cond ((primitive? proc) ;; magic, ignore
         (apply-primeop proc args))
        ((eq (car proc) 'lambda) ;; if is lambda
         (+eval (caddr proc) ;; body
                (bind (cadr proc) ;; vars
                      args
                      env)) ;; env provide from the caller of function
        (t 'error-unknow-procedure))))


;; dynamic binding has serious problems because breaks the problem of modularity
;; the scope is global for all!!!!!!!!!!! because closures will conflict of its internal variables
;; with the caller and so on

;; SO GO TO HELL WITH OF THIS TYPE OF ABSTRACTION HERE!
;; SCHEME AND COMMON LISP DON'T IMPLEMENTS DYNAMIC BINDING
;; (however seems JS do as well old lisp implementations)


;; by other hand, we can use a double closure to take n variable passing to them

;; pgen => procedure generator
(defun pgen (n)
  (lambda (x) (expt x n)))

(defun sum-powers (a b n)
  (sum #'(pgen n)
       a
       #'1+
       b))

(defun product-powers (a b n)
  (product #'(pgen n)
           a
           #'1+
           b))

;; Oh, nice! Sussman talk about the lazy evaluation problem from streams
;; procedures commented in the earlier lectures. Yes, we have a problem passing in the
;; way is showed. The arguments, by default, is evaluated first before function call
;; in Lisp.

(defun +unless (p c a)
  (cond ((not p) c)
        (t a)))

;; so

(+unless (= 1 0) 2 (/ 1 0)) ;; is ok, huh?

;; NOooot! nOOOT! As Pingu would say. That not will return 2, because before that (/ 1 0) is
;; evaluated and that procedure call is giant a sin. A ARITHMETIC ERROR IS TROWED INTO YOUR FACE!


;; but if `c` and `a` was delayed automatically?


;; MODE : SCHEME
#|

(define (unless p (name a) (name c))
  (cond ((not p) a)
        (t c)))

|#

;; if this is implemented, we need change again our interpreter/compiler core

;; primitives for that type of thing
(defun delay (x)
  (lambda () x))

(defun force (x)
  (funcall x))

(defun undelay (x)
  (cond ((and (pair? v)
              (eq (car v) 'thunk))
         (undelay (eval (cadr v)
                        (cddr v))))
        (t v)))

(defun make-delay (exp env)
  (cons 'thunk) (cons exp env))

(defun +eval (exp env)
  (cond ((numberp exp) exp)
        ((symbolp exp) (lookup exp env))
        ((eq (car exp) 'quote) (cadr exp))
        ((eq (car exp) 'lambda)
         (list 'closure (cdr exp) env))
        ((eq (car exp) 'cond)
         (evcond (cdr exp) env))
        (t (+apply (undelay (+eval (car exp) env)) ;; force undelay here
                   (cdr exp)                       ;; btw,  wtf is undelay?
                   env)))) ;; we need that again

(defun evlist (l env)
  (cond ((eq 1 '()) nil)
        (t (cons (undelay (eval (car l env)))
                 (evlist (cdr l) env)))))

(defun gevlist (vars exp env)
  (cond ((eq exps nil) nil)
        ((symbolp (car vars))
         (cons (eval (car exps) env)
               (gevlist (cdr vars)
                        (cdr exps)
                        env)))
        ((eq? (caar vars) 'name)
         (cons (make-delay (car exps) env)
               (gevlist (cdr vars)
                        (cdr exps)
                        env)))
        (t (error 'error-unknown-declaration))))

(defun +apply (proc args env) ;; now apply needs env as parameter!
  (cond ((primitive? proc) ;; magic, ignore
         (apply-primeop proc (evlist args env)))
        ((eq (car proc) 'closure) ;; if is lambda
         ;; proc = (closure ((bvrs) body) env)
         (+eval (cadadr proc) ;; body
                (bind (vnames (caadr proc)) ;; vars
                      (gevlist (caadr proc)
                               args
                               env)
                      (caddr proc))) ;; env provide from the caller of function
         (t 'error-unknow-procedure))))

;; evcond needs change too
(defun evcond (clauses env)
  (cond ((eq clauses '()) '())
        ((eq (caar clauses) t)
         (+eval (cadar clauses) env))
        ((false? (undelay       ;; because need predicate evaluates
                  (+eval (caar clauses)
                         env)))
         (evcond (cdr clauses) env))
        (t (+eval (cadar clauses) env))))



;; THIS FUCKING METACIRCULAR EVALUATOR IS REALLY OBSCURE!!! hahaha
