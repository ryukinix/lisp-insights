;;
;;     MM""""""""`M
;;     MM  mmmmmmmM
;;     M`      MMMM 88d8b.d8b. .d8888b. .d8888b. .d8888b.
;;     MM  MMMMMMMM 88''88'`88 88'  `88 88'  `"" Y8ooooo.
;;     MM  MMMMMMMM 88  88  88 88.  .88 88.  ...       88
;;     MM        .M dP  dP  dP `88888P8 '88888P' '88888P'
;;     MMMMMMMMMMMM
;;
;;         M""MMMMMMMM M""M M""MMMMM""M MM""""""""`M
;;         M  MMMMMMMM M  M M  MMMMM  M MM  mmmmmmmM
;;         M  MMMMMMMM M  M M  MMMMP  M M`      MMMM
;;         M  MMMMMMMM M  M M  MMMM' .M MM  MMMMMMMM
;;         M  MMMMMMMM M  M M  MMP' .MM MM  MMMMMMMM
;;         M         M M  M M     .dMMM MM        .M
;;         MMMMMMMMMMM MMMM MMMMMMMMMMM MMMMMMMMMMMM  Version 1.0beta27
;;
;;           http://github.com/overtone/emacs-live
;;
;; Hello Manoel, it's lovely to see you again. I do hope that you're


;; if conditions

;; returns true
(if '(1)
    :true
    :false)

;; returns true too
(if '()
    :false
    :true)

;; return the length of a list
(defun my-length (list)
  "My function personal length.
   That is a common docstring in common lisp."
  (if list
      (1+ (my-length (cdr list)))
       0))

(my-length (list 1 2 3 4))

;; the history behind the simmetry of nil and empty lists
(and (equal () nil) (equal '() 'nil))
;; the four () disguises
;; () | nil
;; '()| 'nil

;; the four () disguises
;; () | nil
;; '()| 'nil

;; 'yup
(if (= (+ 1 2) 3)
     'yup
     'nope)

;; 'nope
(if (= (+ 1 2) 4)
    'yupq
    'nope)

;; 'the-list-has-stuff-in-it
(if '(1)
    'the-list-has-stuff-in-it
    'the-list-is-empty)

;; the-list-is-empty
(if '()
    'the-list-has-stuff-in-it
    'the-list-is-empty)

(oddp 3)
(evenp 2)


(if (oddp 5)
    'odd-number
    (/ 1 10))


;; progn, global variables and the if 'form' (nothing here is statement, all are 'forms')
(defvar *number-was-odd* nil)

(defun nice-side-effect-lol (x)
  "If odd return the symbol correspondent
  the side-effect is change the global
  variable *number-was-odd*"
  (if (oddp x)
      (progn (setf *number-was-odd* t)
             'odd-number)
      'even-number))

(nice-side-effect-lol 5) *number-was-odd*

;; conditiions;
;; if, when unless command
;; case, cond or

(defvar *number-is-odd* nil)
(when (oddp 5)
      (setf *number-is-odd* t)
       'odd-number)
     'yup
     'nope)

(if (= (+ 1 2) 4)
    'yup
    'nope)

(if '(1)
    'the-list-has-stuff-in-it
    'the-list-is-empty)

(if '()
    'the-list-has-stuff-in-it
    'the-list-is-empty)

(if (oddp 5)
    'odd-number
    (/ 1 10))


;; more examples about if form
(defvar *number-was-odd* nil)

(defun nice-side-effect-lol (x)
  (if (oddp x)
      (progn (setf *number-was-odd* t)
             'odd-number)
      'even-number))

(nice-side-effect-lol 5)

;; conditiions;
;; if, when unless command
;; case, cond or

;; if equivalent -> if-else
;; when -> if block
;; unless -> if not block
;; why use when-unless instead if? Because when
;; don't do nothing in the opposite way

(defvar *number-is-odd* nil)
(when (oddp 5)
  (setf *number-is-odd* t)
  'odd-number) ;; returns odd-number and ste number-is-odd = t

(unless (oddp 4)
  (setf *number-is-odd* nil)
  'even-number) ;; -> even-number set *number-is-odd* = nil

;; the command that does it all: cond
;; the cond form is the classic way
;; to do branching in lisp
;; through the liberal use of parentheses, it allow for an implicit progn,
;; can handle more than one branch, and can even evaluate several conditions
;; in sucession
;; many lispers consider the 'cond' is the one true lisp conditional

(defvar *arch-enemy* nil)
(defun pudding-eater (person)
  (cond ((eq person 'henry) (setf *arch-enemy* 'stupid-lisp-alien)
         '(curse you lisp alien - you ate my pudding))
        ((eq person 'johnny) (setf *arch-enemy* 'useless-old-johnny)
                             '(i hope you choked on my pudding johnny ?))
        (t                   '(why you eat my pudding stranger ?))))


(pudding-eater 'johnny) *arch-enemy*
(pudding-eater 'henery) *arch-enemy*

;; as you can see the cond use a body of parentheses conditions to evaluate
;; a bunch of possible branchs and conditionals
;; is like of sum of when
;; now go re-write the pudding-eater function with case!


(defun pudding-eater (person)
  (case person
        ((henry)   (setf *arch-enemy* 'stupid-lisp-alien)
                   '(curse you lisp alien - you ate my pudding))
        ((johnny)  (setf *arch-enemy* 'useless-old-johnny)
                    '(i hope you choked on my pudding johnny ?))
        (otherwise '(why you eat my pudding stranger ?))))

;; as you can se, the cond and case are really similar, but case
;; differ with one point, doesnt individual form equalities,
;; you choice the case <var> and compares later in the individual branchs
;; with it.


;; now we think about the obscure use of conditionals using
;; only booleans expressions like 'and' and 'or'.

(and (oddp 3) (oddp 5) (oddp 9)) ;=> t
(or (oddp 2) (oddp 0) (oddp 1)) ;=> t


;; if you see that, these operators appears only mathematical boolean operators
;; and nothing about condinitional evaluation. But we had some interesting thing.
;; On really, he can be used for conditional behavior!
;; For instance, here's now a way to set a variable global to t when the number is even.

(defun crazy-evenp (x)
  (let (is-even)
  (or (oddp x) (setf is-even t)) ;; HMM, so black magic
  is-even))

(crazy-evenp 5) ;; -> nil
(crazy-evenp 6) ;; -> T


;; That works because boolean operations are lazy, if doesn't necessary more evaluate the other expressions
;; so we don't evaluate that! For (crazy-evenp 5) returns nil because (oddp 5) is true, as for 'or' operation
;; we need only a uniq true value, the (setf is-even t) is not evaluated. We can call that 'shortcut Boolean evaluation'
;; and lisp use that.

;; Considering that the follow expression can be translated:

(if *file-modified*
    (if (ask-user-about-saving)
        (save-file)))

(and *file-modified* (ask-user-about-saving) (save-file))

;; The and evaluate sequencialy the expressions, but for that, (save-file) needs to returns a t value althoug
;; that kind of function don't explicit mean that, save-file may return other things.
;; We have a problem with that and some lispers can be say is not cool.
;; A third version of that, and a bit more clear, can be:

(if (and *file-modified*
         (ask-user-about-saving))
    (save-file))
