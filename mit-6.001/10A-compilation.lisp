;; Common Lisp Script
;; Manoel Vilela


#|
  Compilation

The first strategy for compilation is the zeroth order compilation. That claims in these simple procedures:

- interpret the code for register machine
- instead execute that, save.

Done. No optimizations here.
But interpreter is dumb, it is very pessimistic because anything can happens.
By other hand, compilers know what is necessary or no (well, it can know).

A lot of stack is used during interpretation, but sometimes is useless.

More details of this lecture is on the Chapter 5 of Structure Interpretation of Computer Programs Book Ed2.
|#

(defmacro define-compilation (name &rest code)
  `(defparameter ,name (quote ,code)))

;; a general view of a compilation of (op a b)
(define-compilation op-a-b
  (preserves env)
  (compile op)
  (result in fun)
  (preserves fun)
  (preserves env)
  (compile a)
  (assign argl (cons (fetch val) nil))
  (result in val)
  (preservs argl)
  (compile b)
  (result in val)
  (assign argl (cons (fetch val) (fetch argl)))
  (goto apply-dispatch))

;; treating assignments and using the stack
;; to preserve registers

;; ===:: APPENDING SEQUENCE OF CODES ::==
;; append seq1 and seq2 preserving reg

;; if seq2 needs reg
;; and seq1 modifies reg
;;; :: CODE
;; (save reg)
;; <seq1>
;; (restore reg)
;; <seq 2>
;; OTHERWISE
;;; :: CODE
;; <seq1>
;; <seq2>


;; sequences of instructions need be tagged about
;; the registers will be modified
;; and the registers needed

;; this is the general notation of code tagging about
;; registers modified and needed
(define-compilation template-for-tagging
  < sequence of instrunctions >
  < set of registers modified >
  < set of regs needed >)

;; lets compile a factorial function recursive

(defun fact (n)
  (cond ((= n 0) 1)
        (t (* n (fact (1- n))))))


(define-compilation fact-compiled
  entry1 ;; label
  (assign env (compiled-procedure-env (fetch fun)))
  (assign env (extended-binding-env '(n)
                                    (fetch argl)
                                    (fetch env)))
  (save env) ;; preserving env, will modify in sequence
  (assign fun (lookup-variable-value '* (fetch env)))
  (assign val (lookup-variable-value 'n (fetch env)))
  (assign argl (cons (fetch val) '()))
  (assign val '0)
  (assign argl (cons (fetch val) (fetch argl))) ;; append argl
  (assign continue after-call3)
  (goto apply-dispatch)

  after-call3 ;; label
  (restore env)
  (branch (true? (fetch val)) true-branch2)
  (assign fun (lookup-variable-value '* (fetch env)))
  (save fun)
  (assign val (lookup-variable-value 'm (fetch env)))
  (assign argl (cons (fetch val) '()))
  (save argl)
  (assign fun (lookup-variable-value 'fact (fetch env)))
  (save fun)
  (assign fun (lookup-variable-value '- (fetch env)))
  (assign val (lookup-variable-value 'n (fetch env)))
  (assign argl (cons (fetch val)) '())
  (assign val '1)
  (assign argl (cons (fetch val (fetch argl))))
  (assign continue after-call5)
  (save continue)
  (goto apply-dispatch)

  after-call5 ;; label
  (assign argl (cons (fetch val) '()))
  (restore fun)
  (assign continue after-call4)
  (save continue)
  (goto apply-dispatch)

  after-call4 ;; label
  (restore argl)
  (assign argl (cons (fetch val) (fetch argl)))
  (restore fun)
  (goto apply-dispatch)

  true-branch2
  (assign val '1)
  (restore continue)
  (goto (fetch continue)))
