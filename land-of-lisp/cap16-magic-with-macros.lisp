;; Common Lisp Script
;; Manoel Vilela

#| MACRO PROGRAMMING

Allows you to mess around inside your Lisp compiler to turn Lisp into
your own custom programming language. When faced with a difficult programming
challenge, many experienced Lispers will first ask themselves:

"What programming language could I use to make this problem easy to solve?"

Then they'll use macros to convert Lisp into that language!
|#

;; A Simple Lisp Macro

;; fancy let1 (simple let)
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(let ((foo (+ 2 3)))
  (* foo foo))

(let1 foo (+ 2 3)
  (* foo foo))

(macroexpand '(let1 foo (+ 2 3)
                 (* foo foo)))
;; .. EXPANSION ..
;; (LET ((FOO (+ 2 3)))
;;    (* FOO FOO))

;; More Complex Macros

;; Warning! Contains Bugs!
(defmacro split (val yes no)
  `(if ,val
       (let ((head (car ,val))
             (tail (cdr ,val)))
         ,yes)
       ,no))

(split '(1 2)
       (format t "head - tail <=> ~a - ~a" head tail)
       (format t "no head tail"))

;; Avoid Repeated Execution in Macros

(macroexpand '(split (progn (princ "Lisp rocks!")
                            '(2 3))
               (format t "This can be split into ~a and ~a." head tail)
               (format t "This cannot be split.")))

#| =>
(IF #1=(PROGN (PRINC "Lisp rocks!") '(2 3))
    (LET ((HEAD (CAR #1#)) (TAIL (CDR #1#)))
      (FORMAT T "This can be split into ~a and ~a." HEAD TAIL))
    (FORMAT T "This cannot be split."))
|#
;; the result above is optimized by SBCL
;; in another implementation, like SBCL, the val will be evaluated
;; two times, at which (princ "Lisp rocks!") will be printed three times

;; explicitly evaluation just one-time of val
;; Warning! Still contains bugs
(defmacro split (val yes no)
  `(let1 x ,val
     (if x
         (let ((head (car x))
               (tail (cdr x)))
           ,yes)
         ,no)))

;; (let1 x 100
;;   (split '(2 3)
;;          (+ x head)
;;          nil))
;; ERROR! '(2 3) is not a number

(macroexpand '(split '(2 3)
                      (+ x head)
                      nil))
#| EXPANSION

(LET ((X '(2 3)))
  (IF X
      (LET ((HEAD (CAR X)) (TAIL (CDR X)))
        (+ X HEAD))
      NIL))
|#

;; So x <- 100 receive the new states '(2 3)
;; after the 'split macro expansion.
;; Pretty bad.


;; Use of gensym

;; This function is finally safe to use
(defmacro split (val yes no)
  (let1 g (gensym)
    `(let1 ,g ,val
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
             ,yes)
           ,no))))


(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail)
                           (cons (cons head (car tail))
                                 acc))
                        (reverse acc))
                    (reverse acc))))
    (f lst nil)))

(pairs '(a b c d e f))
;; => ((A . B) (C . D) (E . F))

(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
    `(labels ((self ,(mapcar #'car p)
                ,@body))
       (self ,@(mapcar #'cdr p)))))

(recurse (n 10)
  (fresh-line)
  (if (= n 1)
      (princ "CANCER")
      (progn (princ n)
             (self (1- n)))))

;; my-length function using black magic macros
(defun my-length (lst)
  (recurse (lst lst
            acc 0)
    (split lst
           (self tail (1+ acc))
           acc)))

(my-length '(1 2 3))

;; functional way (alternative to macro techniques)
(defun my-length (lst)
  (reduce (lambda (x _)
            (declare (ignore _))
            (1+ x))
          lst
          :initial-value 0))
