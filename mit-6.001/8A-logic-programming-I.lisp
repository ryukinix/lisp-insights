;; Common Lisp Script
;; Manoel Vilela

#|
   Some explorations on Logic Programming and Declarative Programming

   This lecture don't cover the implementation of the query system based on logic!
|#

;; (+merge '(4 5 6) '(1 2 3)) => '(1 2 3 4 5 6)
(defun +merge (x y)
  (cond ((null x) y)
        ((null y) x)
        (t (let ((a (car x))
                 (b (car y)))
             (if (< a b)
                 (cons a
                       (+merge (cdr x) y))
                 (cons b
                       (+merge x (cdr y))))))))

;; in logic

#|
  (1 3 7) and (2 4 1) merge-to-form ?
  (1 3 7) and ? merge-to-form (1 2 3 4 7 8)
  x? and y? merge-to-form (1 2 3 4 7 8)

  Declarative properties, logical thinking.

  The usage of Logic Programming (e.g.:: Prolog) is three:

  1. To express what is true
  2. Check whether something is true
  3. Find what's is true

|#

;; this macros don't exists on lectures, i put here just for fun and to the file compiles.

(defvar jobs nil)
(defvar salaries nil)
(defvar supervisors nil)
(defvar addresses nil)

(defmacro job (who job)
  `(push (cons (quote ,who) (quote ,job)) jobs))

(defmacro salary (who salary)
  `(push (cons (quote ,who) (quote ,salary)) salaries))

(defmacro supervisor (who s)
  `(push (cons (quote ,who) (quote  ,s)) supervisors))

(defmacro address (who a)
  `(push (cons (quote ,who) (quote  ,a)) addresses))


;; entry 1
(job (Bitdiddle Ben) (computer wizard))

(salary (Bitdiddle Ben) 40000)

(supervisor (Bitdiddle Ben)
            (Warbucks Oliver))

(address (Bitdiddle Ben)
         (Slunerville (Ridge Road) 10))

;; entry 2
(job (Hacker Alyssa P)
     (computer programmer))

(salary (Hacker Alyssa P) 35000)

(supervisor (Hacker Alyssa P)
            (Bitdiddle Ben))

(address (Hacker Alyssa P)
         (Cambridge (Mass Ave) 78))

;; entry 3
(job (Tweakit Lan E)
     (computer technician))


(salary (Tweakit Len E) 15000)

(supervisor (Tweakit Len E)
            (Bitdiddle Ben))

(address (Tweakit Len E)
         (Boston (Bay State Road) 22))

;; entry 4

(job (Reasoner Louis)
     (computer programmer trainee))

(salary (Reasoner Louis) 20000)

(supervisor (Reasoner Louis)
            (Hacker Alyssa P))

(address (Reasoner Louis)
         (Slunerville (Pine Tree Road)
                      80))


;; primitives =>
;; * query
;; ~ like the data entry above

;; means of combination =>
;; * and
;; * not
;; * or
;; * lisp-value

;; (and (job ?x (computer . ?y))
;;      (not (and (supervisor ?x ?z)
;;                (job ?z (computer . ?w))))

;; means of abstraction =>
;; * rules

;; (rule (bigshot ?x ?dept) ;; conclusion
;;       (and (job ?x (?dept . ?y)) ;; body
;;            (not (and (supervisor ?x ?z)
;;                 (job ?z (?dept . ?w))))))

;; backing to the merge problem

;; (rule (merge-to-form () ?y ?y))
;; (rule (merge-to-form ?y () ?y))

#|

(rule
 (merge-to-form (?a . ?x) (?b . ?y) (?b . ?z))
 (and (merge-to-form (?a . ?x) ?y ?z)
      (lisp-value > ?a ?b)))

|#
