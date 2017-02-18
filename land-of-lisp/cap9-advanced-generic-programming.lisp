;; Common Lisp Script
;; Manoel Vilela

;; advanced datatypes:
;; + ARRAY
;; + HASH-TABLES
;; + STRINGS
;; + STRUCTURES

;; generic functions:
;;   sequence functions:
;;     + REDUCE
;;     + FIND-IF
;;     + SOME
;;     + EVERY
;;     + POSITION
;;     + COUNT
;;   generic setter:
;;     + SETF

;; best way to handle various datatypes: DEFMETHOD

(defparameter examples nil)

(defmacro define-example (name &rest body)
  `(push (cons (quote ,name)
               (apply #'list (quote ,body))) examples))

(define-example example-of-arrays
  (make-array 3)
  ;; => #(0 0 0)
  (defparameter x (make-array 3))
  ;; x => #(0 0 0)
  (aref x 1) ;; => 0

  ;; the generic setf macro operator
  (setf (aref x 1) 'foo)
  ;; x => #(0 FOO 0)
  ;; combining setf + aref we can set elements on
  ;; array positions

  ;; but this can be used in list too!
  ;; yes... lists of common lisp is not immutable!
  ;; functional programming? forget about that.

  (defparameter foo '(a b c))
  foo
  ;; foo => (a b c)
  (second foo)
  ;; => B
  (setf (second foo) 'z)
  foo
  ;; => (a z c)

  ;; setf is a magic setter.
  (defparameter foo (make-array 4))
  foo
  ;; => #(0 0 0 0)

  (setf (aref foo 2) '(x y z))
  foo
  ;; => #(0 0 (X Y Z) 0)

  (setf (car (aref foo 2)) (make-hash-table))
  ;; hash tables has its own examples in the next section

  (setf (gethash 'zoink (car (aref foo 2))) 5)
  foo
  ;; => #(0 0 (#<HASH-TABLE :TEST EQL :COUNT 1 {100382B253}> Y Z) 0)
  )

(define-example array-vs-lists
  (nth 1 '(foo bar baz))
  ;; access O(n)

  (aref #(foo bar baz) 1)
  ;; access O(1)

  ;; tl;dr => array has a better performance than lists

  )


;; hash tables are like alists (assoc lists) => ((key value) ... ... ... (key-n value-n))
(define-example example-of-hash-tables
  ;; create a new hash table
  (make-hash-table)
  ;; => #<HASH-TABLE :TEST EQL :COUNT 0 {10049714B3}>
  (defparameter x (make-hash-table))
  (gethash 'yup x)
  ;; => NIL, NIL
  (setf (gethash 'yup x) '25)
  (gethash 'yup x)
  ;; => 25, T

  ;; defining drink-order example like alist made before
  (defparameter *drink-order* (make-hash-table))
  (setf (gethash 'bill *drink-order*) 'double-espresso)
  (setf (gethash 'lisa *drink-order*) 'small-drip-coffee)
  (setf (gethash 'john *drink-order*) 'medium-latte)

  ;; accessing the drink order for any person
  (gethash 'lisa *drink-order*)
  ;; hash table access performance is like array
  ;; access/set time -> O(1)
  )

(define-example returning-multiple-values
  (round 2.4)
  ;; => 2, 0.4
  (defun foo () (values 3 7))
  (foo)
  ;; => 3, 7
  (+ (foo) 5)
  ;; => 8 (ignoring the second value)

  ;; bind multiple values from functions and create
  ;; a lexical scope like let
  (multiple-value-bind (a b) (foo)
    (+ a b))
  ;; => 10

  ;; the usage of multiple-values is supported by CL
  ;; but is not so common used on moderns lisp dialects
  ;; like clojure

  ;; BTW, Land of Lisp explicitly say that on this book
  ;; we don't will see much examples of this.

  ;; I don't have sure if this is really useful.
  ;; Maybe passing error state like Go (value error)
  )

(define-example example-of-structures
  (defstruct person
    :name
    :age
    :waist-size
    :favorite-color) ;; 4 slots
  (defparameter *bob* (make-person :name "Bob"
                                   :age 35
                                   :waist-size 32
                                   :favorite-color "blue"))
  *bob*
  ;; => #S(PERSON :NAME "Bob" :AGE 35 :WAIST-SIZE 32 :FAVORITE-COLOR "blue")
  (person-age *bob*)
  ;; => 35
  (setf (person-age *bob*) 36) ;; the magic setter macro again
  ;; works fine with structures as well
  (person-age *bob*)
  ;; => 36


  ;; the problem of lispers and object-oriented programming
  ;; let's see a alternative for structures only using lists
  (defun make-person. (name age waist-size favorite-color)
    (list name age waist-size favorite-color))
  (defun person-age. (person)
    (cadr person))
  (defparameter *bob* (make-person. "bob" 35 32 "blue"))
  *bob*
  (person-age. *bob*)

  ;; but is a bad idea. we'll need all the selectors for person
  ;; and the REPL representation is useless. How we can say if
  ;; this list define a person. Bob's age is 35 or 32?
  ;; Another problem is changing the state... lists don't works well
  ;; with it. First: your nature is a bunch of recursive cons cells.
  ;; In that example the better approach is defining structures in CL.

  )


(define-example example-handling-data-in-a-generic-way
  ;; a great example of generic function
  ;; at which can handle various types is the
  ;; function length
  ;; that type of functions are called of:
  ;; "sequence functions" (handle sequences)
  (length "blub")
  ;; => 4
  (length '(a b c))
  ;; => 3
  (length (make-array 5))
  ;; => 5

  ;; another great examples for sequence functions
  ;; are the specific for search:
  ;; find-if, count, position, some and every

  (find-if #'numberp '(a b 5 d))
  ;; => 5
  (count #\s "Mississippi")
  ;; => 4
  (position #\4 "2kewl4skewl")
  ;; => 5
  (some #'numberp '(a b 5 d))
  ;; => T
  (every #'numberp '(a b 5 d))
  ;; => NIL

  ;; another useful generic sequence function: reduce
  (reduce #'+ '(3 4 6 5 2))
  ;; => 20
  ;; an way to understand the evaluation of reduce is
  (reduce #'cons '(1 2 3 4))
  ;; => (((1 . 2) . 3) . 4)
  (reduce (lambda (best item)
            (if (and (evenp item)
                     (> item best))
                item
                best))
          '(7 4 6 5 2)
          :initial-value 0)
  ;; => 6
  ;; without define the initial-value we got 7


  (defun sum (sequence)
    (reduce #'+ sequence))

  (sum '(1 2 3))
  (sum #(1 2 3 4 5))

  (map 'string
       (lambda (x)
         (if (eq x #\s)
             #\S
             x))
       "this is a string")

  ;; two more import sequence functions: subseq and
  ;; sort

  (subseq "america" 2 6)
  ;; => eric
  (sort '(5 8 2 4 9 3 6) #'<)
  ;; => (2 3 4 5 6 8 9)

  ;; we can create our own generic functions
  ;; using typing checking; the most frequently
  ;; predicates are: arrayp, characterp, consp,
  ;; hash-table-p, listp, stringp, symbolp

  (defun add (a b)
    (cond ((and (numberp a)
                (numberp b))
           (+ a b))
          ((and (listp a)
                (listp b))
           (append a b))))

  (add 1 2)
  ;; => 3
  (add '(1 2) '(3 4))
  ;; => (1 2 3 4)

  ;; but we have a better way to do this using
  ;; generic methods for multiple types
  ;; this is called "type dispatching"

  (defmethod add. ((a number) (b number))
    (+ a b))

  (defmethod add. ((a list) (b list))
    (append a b))

  (add. 1 2)
  (add. '(a b) '(c d))


  ;; the `defmethod` is like `defun` except that it
  ;; allows us to write multiple functions with
  ;; the same name

  ;; defstruct + defmethod => simple OO system
  )


(defun print-example (example)
  (let ((name (car example))
        (instrunctions (cdr example)))
    (format t ":: ~A ~%" name)
    (loop for i in instrunctions do
      (format t "> ~a~%~a~%" i (eval i)))
    (princ #\newline)))

(defun run-examples ()
  (mapcar #'print-example (reverse examples)))


;; the continuation of this chapter is a game called:
;; ORC BATTLE GAME
;; I'll write the functions in another file because
;; this is especial of only REPL examples at which
;; I use the define-example macro to can be executable


(run-examples)
