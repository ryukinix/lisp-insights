;; Common Lisp Script
;; Manoel Vilela


(defpackage :text-game-engine
  (:use :cl))

(in-package :text-game-engine)

;; node (description)
(defparameter *nodes* '((living-room (you are in the living-room.
                                      a wizard is snoring loudly on the couch.))
                        (garden (you are in a beatiful garden.
                                 there is a well in front of you.))
                        (attic (you are in the attic.
                                there os a giant welding torch in the corner.))))


;; (node (edge direction way))
(defparameter *edges* '((living-room (garden west door)
                                    (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))


;; set of objects
(defparameter *objects* '(whiskey bucket frog chain shit gold))


;; (object location)
(defparameter *objects-locations* '((whiskey living-room)
                                    (bucket living-room)
                                    (chain garden)
                                    (frog garden)
                                    (shit attic)
                                    (gold attic)))


;; we can put the itens of inventory at *objects-locations*
;; but i decide is better use *inventory* to split that
;; and is more easily to  handle on the functions about pickup-drop
(defparameter *inventory* nil)


;; that variable handle the actual location of the player
(defparameter *location* 'living-room)


;; commands whose the user can be type at repl
(defparameter *allowed-commands* '(inventory look walk pickup drop))

;; a lot of tests for running
;; on the end of that script
(defparameter *tests* '((describe-path '(garden west door))
                        (describe-location 'living-room *nodes*)
                        (describe-paths 'living-room *edges*)
                        (objects-at 'living-room *objects* *objects-locations*)
                        (describe-objects 'living-room *objects* *objects-locations*)
                        (look)
                        (pickup 'whiskey)
                        (inventory)
                        (look)
                        (drop 'whiskey)
                        (inventory)
                        (look)
                        (drop 'whiskey)
                        (inventory)
                        (look)
                        (walk 'upstairs)
                        (pickup 'shit)
                        (pickup 'gold)
                        (drop 'shit)
                        (inventory)
                        (look)))


;; nice, functional! no side-effections
(defun describe-location (location nodes)
  "Sucint description of the localation provide
   @location -> symbol
   @nodes -> association list"
  (cadr (assoc location nodes)))


;; functional! no side-effects
(defun describe-path (edge)
  "Sucint description of the path to achieve.
   @edge -> (neighbor direction way)"
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))


;; functional! no side-effects
(defun describe-paths (location edges)
  "General description of the possible edges on each location
   @location -> symbol
   @edges -> association list of `(node (neighbor direction way))"
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))


;; functional! no side-effects
(defun objects-at (loc objs obj-locs)
  "Get the objects inside the location
   @loc -> location symbol
   @objs -> set of objects
   @obj-locs -> the list with (object location)"
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))


;; functional! no side-effects
(defun describe-objects (loc objs obj-loc)
  "Describe the objects existent of the location
   @loc -> location symbol
   @objs -> set of objects
   @obj-locs -> the list with (object location)

   That function is a wrapper of objects-at printing
   A beautiful description of object on the location."
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))


;; i like that, but is not functional
;; appears don't have side-effects, we don't have a same
;; output with the same args, because use global variables
(defun look ()
  "Look function, do a general descriptions of your location.
   The location, the paths and objects."
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *objects-locations*)))


;; nice function! but not functional
;; variable globals: *location* *edges*
;; side-effect: change *location*
(defun walk (direction)
  "Try change the actual location moving to direction and
   achieve the next scenario if is possible.
   @direction -> a symbol as possible move: 'west, 'east', 'north, 'sul"
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way.))))


;; variable globals: *objects-locations*, *inventory*
;; side-effect: change *objects-locations*, *inventory*
(defun pickup (object)
  "Try get a object at the current scenarion *location*
   Store at *inventory* and remove from *objects-location*
   remove from *objects* to... (but i don't like that)
   *objects* your purpose is a set of all objects on the game
   @object -> a symbol"
  (cond ((member object
                 (objects-at *location* *objects* *objects-locations*))
         (progn (setf *objects-locations*
                      (remove-if
                        #'(lambda (x) (eq (car x) object))
                        *objects-locations*))
                (push object *inventory*))
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))


;; variable globals: *objects-locations*, *inventory*
;; side-effect: change *objects-locations*, *inventory*
(defun drop (object)
  "Drop a existent object on the invetory at the actual location
   @object -> a symbol"
  (if (member object *inventory*)
      (progn (push (list object *location*) *objects-locations*)
             (setf *inventory* (remove object *inventory*))
             `(you dropped ,object at ,*location*))
      `(you do not have ,object on your inventory)))


;; variable globals: *inventory*
(defun inventory ()
  "Show the inventory at actual carrying items"
  (cons 'items-  *inventory*))

;; first version: simple
(defun game-repl-noob () ;; useless
  (loop (print (eval (read)))))

;; wishful thinking
(defun game-repl ()
  "The game-repl protecting
   the user for black magic of
   lisp repl"
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

;; now we need define:
;; game-print, game-read, game-eval

(defun game-read ()
  "Read an expressions without () from stdin
   and return the expression with ().
   Example: walk lest -> (walk 'lest)"
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defun game-eval (sexp)
  "Eval only commands alloweds"
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command)))

;; i need understand better that black magic below
(defun tweak-text (lst caps lit)
  "Make a correct captilize in a list of symbols"
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eq item #\") (tweak-text rest caps (not lit)))
             (lit (cons item (tweak-text rest nil lit)))
            ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))

;; no side effect
(defun help ()
  "Show a descriptions of possible commands to interact
   with the lisp world."
  '(options -> (look) (walk ?direction) (inventory) (pickup ?object) (drop ?object)))

(defun eval-printing (command)
  "The logic behind the scenes is:
   Print the command, so eval the command printed
   and print the output"
  (print (cons 'command-execute-> `(,command)))
  (print (cons 'output-of-command-> (eval command))))

(defun run-tests (tests)
  (mapcar #'eval-printing tests))

(eval-when (:compile-toplevel)
  (run-tests *tests*))
