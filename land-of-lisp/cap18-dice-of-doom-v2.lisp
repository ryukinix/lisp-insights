;; Common Lisp Script
;; Manoel Vilela

(load "cap15-dice-of-doom") ;; load as package dice-doom
(load "cap18-lazy-programming") ;; load directly to cl-user

(defpackage :dice-of-doom-v2
  (:use :cl :dice-of-doom :lazy)
  (:export :human
           :computer
           :*num-players*
           :*max-dice*
           :*board-size*
           :*board-hexnum*))

(in-package dice-of-doom-v2)
