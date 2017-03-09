;; Common Lisp Script
;; Manoel Vilela

;; :: Signaling a Condition

;; Throwing a error:
(error "foo")
;; => The REPL is interrupted
;; *** - foo

;; Using the error command will interrupt your running Lisp program
;; unless you intercept the error elsewhere to prevent an interruption.

;; :: Creating Custom Conditions

;; A more sophisticated way to signal conditions is to first
;; define a custom condition using `define-condition`, as here:

(define-condition foo () ()
  (:report (lambda (condition stream)
             (princ "Stop FOOing around, numbskull!" stream))))

(error 'foo)

;; => The REPL is interrupted
;; *** - Stop FOOing around, numbskull!

;; As you can see, our custom message was printed. This technique allows
;; the programmer to get a more meaningful error report, customized for the
;; specific condition that was triggered.

;; :: Intercepting Conditions

(defun bad-function ()
  (error 'foo))

(handler-case (bad-function)
  (foo () "somebody signaled foo!")
  (bar () "somebody signaled bar!"))

;; => "somebody signaled foo!"
;; Our handler-case command intercepts the foo condition that was be
;; signaled through the bad-function call. This means that the program
;; can keep running without interruption, with the handler-case evaluating
;; as "somebody signaled foo!"

;; :: Protecting Resources Against Unexpected Conditions
;; We can ignore exceptions too, like the unsafe operations of Rust.
;; Is like say "This piece of code must run no matter what happens"
;; To the Lisp compiler

(unwind-protect (/ 1 0)
                (princ "I need to say 'flubyduby' matter what"))

;; Actually the exception is signaled and a interruption is made, but,
;; the other statements will still be executed.

;; => DIVISION-BY-ZERO error
;; ... after abort
;; => I need to say 'flubyduby' matter what

;; Within the unwind-protect, we divide by 0, which signals a condition. But
;; even after we tell to compiler to abort, the program still prints its
;; crucial message

;; We can usually avoid calling unwind-protect directly by relying on Common
;; Lisp "with-" macros; many of these call unwind-protect themselves, under
;; the hood.

;; NOTE: In the comic book epilogue at the end of the book, you'll learn
;; about and additional feature of the Common Lisp signaling system called
;; `restarts`.

;; The continuation of this chapter is written on the "cap13-webserver.lisp"
