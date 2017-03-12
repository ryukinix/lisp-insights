;; Common Lisp Script
;; Manoel Vilela


#|
-- Functional Programming

  In this chapter, you're going to learn about the first advanced
Lisp concept, called the functional programming technique.

Here are some important properties of mathematical functions that
we'll want our Lisp functions to obey as well:

+ The function always returns the same result, as long as the same
  argument are passed into it. (This is often referred to as referential
  transparency)
+ The function never references variables that are defined outside the
  function, unless we are certain that these variables will remain
  constant
+ No variables are modified (or mutated, as functional programmers like
  to say) by the function.
+ The purpose of the function is to do nothing other than to return a
  result
+ The function doesn't do anything that is visible to the outside world,
  such as pop up a dialog box on the screen or make your computer go
  "Bing!"
+ The function doesn't take information from an outside source, such as
  the keyboard or the hard drive.

|#


;; a great example of true mathematical function is the sine function

(sin 0.5)
;; => 0.47942555


;; Whenever a piece of code does something that is visible to the outside
;; world, such as go "Bing!" or display a dialog box on the screen,
;; we say that the code causes a side effect.
;; Functional programmers think of such side effects as making your code
;; "dirty"


;; Let's give an example of a functional style program:

;; the clean, functional part
(defun add-widget (database widget)
  (cons widget database))

;; the dirty, non-functional part
(defparameter *database* nil)
(defun main-loop ()
  (loop (princ "Please enter the name of a new widget: ")
        (setf *database* (add-widget *database* (read)))
        (format t "The database contains the following: ~{~a~^, ~}~%" *database*)))


;; NOTE: Some programming languages are even more focused on fp than Lisp is.
;; Haskell, for instance, has powerful features that let you write 99.9% of
;; your code in a functional style. In the end, however, your program will still
;; need to have some kind of side effect; otherwise, your code couldn't
;; accomplish anything useful.


;; :: Higher-Order Programming
;; The most powerful tool for code composition when writing functional
;; code is higher-order programming which lets you use functions that
;; accept other function as parameters.

;; Comparison between imperative style and functional style

;; -> imperative mode, add 2 to each element
(defparameter *my-list* '(4 7 2 3) "a useless list")
(loop for n below (length *my-list*)
      do (setf (nth n *my-list*) (+ (nth n *my-list*) 2)))
;; => NIL
*my-list* ;; => (6 9 4 5)

;; + memory efficient (don't allocated new data)
;; + time efficient
;; - destroy data

;;; -> Using the functional style
(defun add-two (list)
  (when list
    (cons (+ 2 (car list))
          (add-two (cdr list)))))

(add-two '(4 7 2 3))
;; => '(6 9 4 5)

;; - memory inefficient (create new data)
;; + time efficient
;; + don't destroy data

;; Using Higher-Order functions
(mapcar (lambda (x)
          (+ x 2))
        '(4 7 2 3))
;; => '(6 9 4 5)

;; Benefits of Functional Programming:
;; + Functional programming reduces bugs
;; + Functional programs are more compact
;; + Functional code is more elegant

;; Problems of Functional Programming:
;; - In general is inefficient¹


;; ¹: for that, fp programmers, created techniques like:
;; Memoization, Tail Call Optimization, Higher-Order Programming
;; and another things.
;; Using good techniques a functional program can reach the same
;; performance of any other style.
