;; Common Lisp Script
;; Manoel Vilela

(defparameter *nodes* '((living-room (you are in the living-room.
                                      a wizard is snoring loudly on the couch.))
                        (garden (you are in a beatiful garden.
                                 there is a well in front of you.))
                        (attic (you are in the attic.
                                there os a goamt weçdomg torch in the corner.))))

(defparameter *edges* '((living-room (garden west door)
                                    (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *objects-location* '((whiskey living-room)
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
  `(there is a, (caddr edge) going, (cadr edge) from here.))

(defun describe-paths (location edges)
  "General description of the possible edges on each location
   @location -> symbol
   @edges -> association list of `(node (neighbor direction way))"
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))q


;; func tests
(describe-path '(garden west door))
(describe-location 'living-room *nodes*)
(describe-paths 'living-room *edges*)
