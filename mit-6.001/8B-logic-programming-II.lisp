;; Common Lisp Script
;; Manoel Vilela

;; sample patterns

#|

(a ?x c)

(job ?x (computer ?y))

(job ?x (computer . ?y))

(a ?x ?x)

(?x ?y ?y ?x)

(a . ?x)

|#

;; rule-based system for logic programming using
;; patter matching

(load "4A-pattern-matching.lisp")
(match pat data dictionary) ;; needs implements match
;; in earlier lectures, the primitives for pattern matching
;; is implemented

;; query black box

;;                 ↓ pattern
;;                 +-------+
;;                 |       |
;; => initial-dict | QUERY | => dictionary
;;                 |       |
;;                 +-------+
;;                     ↑ database stream


;; means of combinations: NOT, AND, OR

;; means of abstraction: rules

(rule (boss ?z ?d)
      (and (job ?x (?d . ?y))
	   (supervisor (?x ?z))))


;; == TO APPLY A RULE

;; Evaluate the rule body relative to an environment
;; formed by unifying the rule conclusion with the
;; given query.


;; == TO APPLY A PROCEDURE

;; Evaluate the procedure body relative to an enviroment
;; formed by binding the procedure paramaters to the
;; to the arguments.


;; All humans are mortals
;; All greeks are humans.
;; Socrates is greek,
;;------ syllogism logic ----
;; :: Socrates is mortal.


(Greek Socrates)
(Greek Plato)
(Greek Zeus)
(god Zeus)

(rule (mortal ?x) (human ?x))
(rule (fallible ?x) (human ?x))

(rule (human ?x)
      (and (Greek ?x) (not (god ?x))))

(rule (address ?x Olympus)
      (and (greek ?x) (god ?x)))

(rule (perfect ?x)
      (and (not (mortal ?x))
	   (not (fallible ?x))))

(and (address ?x ?y)
     (perfect ?x)) ;; => Mount Olympus (Zeus)

(and (perfect ?x)
     (address ?x ?y)) ;; Nothing


;; But who is right? We can makes assumption of our
;; data here about that: Zeus is not mortal, Zeus
;; is not mortal, just because he is not human?
;; This is not sufficient information.

;; NOT here is NOT from the logic!
;; The NOT here is a filter of a closed world,
;; the complements of a assumption.
;; Logic makes assumptions of only two states.
;; This is a big problem.
