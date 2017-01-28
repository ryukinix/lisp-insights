;; Common Lisp Script
;; Manoel Vilela

;; A review about language design using Lisp, what we've done until now:
;; Picture language by Peter Henderson
;; Digital Logic Language
;; Query Language (Logic Programming like Prolog)

;; CONCRETE COMPUTATION HERE!


;; ======> LISP

;; Meta-circular evaluator was based on Lisp for Lisp. Eval/Apply solving fixed-point equations


;; This stuff seems magic, but right now we'll destroy all the magic using the register machine

;; again, primitives: DATA-PATHS, FINITE-STATE CONTROLLER AND STACK



;; lisp-user -> chars => reader => lisp memory structure => eval =>
;;           primitive-operations => printer => lisp-user

#|
  :: REGISTER USAGE IN EVALUATOR MACHINE ::

EXP         expression to be evaluated
ENV         evaluation environment

FUN         procedure to be applied
ARGL        list of evaluated arguments

CONTINUE    place to go to next

VAL         result of evaluation

UNEV       temporary register for expressions

|#

#|
    SAMPLE EVALUATOR-MACHINE OPERATIONS
(assign val (fetch exp))

(branch (conditional? (fetch exp))
        ev-cond)

(assign exp (first-clause (fetch-exp)))

(assign val (look-variable-value (fetch exp)
                                 (fetch env)))
|#


;; evaluator for LISP in LISP
;; using abstract syntax from SICP book (little different of lecture from Gerald)
(defun eval. (exp env)
  (cond ((self-evaluating ? exp) exp)
        ((quoted? exp)
         (text-of-quotation exp))
        ...
        ((application? exp)
        (apply
         (eval (operator exp) env)
         (list-of-values (operands exp)
                         env)))
        (t (error 'unknown-expression))))


(defun apply. (proc args)
  (cond ((primitive-proc? proc)
         (primitive-apply proc args))
        ((compound-proc? proc)
         (eval-sequence (proc-body proc)
                        (extend-environment
                         (parameters proc)
                         args
                         (proc-environment proc))))
        (t (error unknown-proc-type))))


;; eval/apply cicle :: eval => procedure, arguments =>
;;                     apply => expression, environment =>
;;                     eval


#|
  :: CONTRACT THAT EVAL-DISPATCH FULFILLS ::

- The EXP register holds an expression to
be evaluated;

- The ENV register holds the environment in which the expression
is to be evaluated;

- The CONTINUE register holds a place to go to next;

- The result will be left in the VAL register. Contents of all
other registers may be destroyed;

|#


#|

  :: CONTRACT THAT APPLY-DISPATCH FULFILLS ::

- The ARGL register contains a list of arguments;

- The FUN register contains a procedure to be applied;

- The top of the STACK holds a place to go to next;

- The result will be left in the VAL register. The stack will be
popped. Contents of all other registers may be destroyed;

|#

(load "9A-register-machines.lisp")

(define-machine eval-dispatch
  (branch (self-evaluating? (fetch exp))
            ev-self-eval)
  (branch (variable? (fetch exp))
          ev-variable)

  < more especial forms >

  (branch (application? (fetch exp))
          ev-appliations)
  (goto unknow-expression-error))

(define-machine ev-self-eval
  (assign val (fetch exp))
  (goto (fetch continue)))


(define-machine ev-variable
  (assign val (lookup-variable-value (fetch exp)))
  (goto (fetch continue)))


(define-machine ev-application
  (assign unev (operands (fetch exp)))
  (assign exp (oeprator (fetch exp)))
  (save continue)
  (save env)
  (save unev)
  (assign continu eval-args)
  (goto eval-dispatch))


(define-machine eval-args
  (restore unev)
  (restore env)
  (assign fun (fetch val))
  (save fun)
  (assign argl '())
  (goto eval-arg-loop))

(define-machine eval-arg-loop
  (save argl)
  (assign exp (first-operand (fetch unev)))
  (branch (last-operand? (fetch (unev)))
          (eval-last-arg))
  (save env)
  (save unev)
  (assign continue accumulate-arg)
  (goto eval-dispatch))

(define-machine accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (cons (fetch val)
                     (fetch arg1)))
  (assign unev (rest-operands (fetch unev)))
  (goto eval-arg-loop))

(define-machine apply-dispatch
  (branch (primitive-proc? (fetch fun)
                           primitive-apply))
  (branch (compound-proc? (fetch fun)
                          compound-apply))
  (goto unknown-proc-type-error))


(define-machine primitive-apply
    (assign val (apply-primitive-proc (fetch fun)
                                      (fetch argl)))
  (restore continue)
  (goto (fetch continue)))


(define-machine compound-apply
  (assign exp (procedure-body (fetch fun)))
  (assign env (make-bindings (fetch fun)
                             (fetch arg1)))
  (restore continue)
  (goto eval-dispatch))

(defun f (a b)
  (+ a b))
;; <e0>
(defvar x 3)
(defvar y 4)

(f x y)

(defun fact-rec (n)
  (if (<= n 1)
      1
      (* n
         (fact-rec (1- n)))))

