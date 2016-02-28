;; Common Lisp Script
;; Manoel Vilela

;; node (description)
(defparameter *nodes* '((living-room (you are in the living-room.
                                      a wizard is snoring loudly on the couch.))
                        (garden (you are in a beatiful garden.
                                 there is a well in front of you.))
                        (attic (you are in the attic.
                                there os a goamt weçdomg torch in the corner.))))

;; (node (edge direction way))
(defparameter *edges* '((living-room (garden west door)
                                    (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

;; set of objects
(defparameter *objects* '(whiskey bucket frog chain))


;; (object location)
(defparameter *objects-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))


(defun describe-location (location nodes)
  "Sucint description of the localation provide
   @location -> symbol
   @nodes -> association list"
  (cadr (assoc location nodes)))

(defun describe-path (edge)
  "Sucint description of the path to achieve.
   @edge -> (neighbor direction way)"
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  "General description of the possible edges on each location
   @location -> symbol
   @edges -> association list of `(node (neighbor direction way))"
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (loc objs obj-locs)
  "Get the objects inside the location
   @loc -> location symbol
   @objs -> set of objects
   @obj-locs -> the list with (object location)"
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

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

;; func tests
(describe-path '(garden west door))
(describe-location 'living-room *nodes*)
(describe-paths 'living-room *edges*)
(objects-at 'living-room *objects* *objects-locations*)
(describe-objects 'living-room *objects* *objects-locations*)
