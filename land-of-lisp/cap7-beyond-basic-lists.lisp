;; Common Lisp Script
;; Manoel Vilela

(defpackage :graph-util
  (:use :cl)
  (:export :graph->png
           :ugraph->png
           :main))

(in-package :graph-util)


;; associative lists
(defparameter *drink-order* '((bill . double-espresso)
                              (lisa . samll-drip-coffee)
                              (john . medium-latte)))

;; visualizing tree-like data
(defparameter *house* '((walls (mortar (cement)
                                       (water)
                                       (sand))
                               (bricks))
                        (windows (glass)
                                 (frame)
                                 (curtains)
                        (roof (shingles)
                              (chinmey)))))
;; this is in someway can be hard to visualize the relations of data


;; lets create a graph
(defparameter *wizard-nodes* '((living-room (you are in the living-room.
                               a wizard is snoring loudly on the couch.))
                               (garden (you are in a beatiful garden.
                               there is a wall in front of you.))
                               (attic (you are in the attic. there
                                is a giant welding torch in the corner.))))

(defparameter *wizard-edges* '((living-room (garden west door)
                                            (attic upstairs ladder))
                               (garden (living-room east door))
                               (attic (living-room downstairs ladder))))


(defparameter *max-label-length* 30)

;; * generating the dot information


;; ** converting node identifiers

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))


(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil))) ;; :pretty nil avoid modify the original exp
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))


(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))


(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line) ;; wtf is that?
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))


(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))


(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                   fname
                   :direction :output
                   :if-exists :supersede)
    (funcall thunk))
  ;; generate graph using fname calling dot
  (sb-ext:run-program "dot" (list "-Tpng" "-O" fname) :search t :wait t)

  ;; delete the file
  (sb-ext:run-program "rm" (list fname) :search t :wait t))


;; thunk definition: nullary functions, with zero arguments
;; can be called suspension too


;; note: symbols with prefixed colon are constants, like => :direction :output and so on

;; (let ((:cigar 5))
;;   :cigar)
;; =>
;; Compile-time error:
;; :CIGAR is a keyword, and cannot be used as a local variable.
;; [Condition of type SB-INT:COMPILED-PROGRAM-ERROR]


(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))


;; creating undirected graphs

(defun uedges->dot (edges)
  (maplist (lambda (lst) ;; ? maplist?
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar lst)))
           edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))

(defun main()

  (in-package :graph-util)
  ;; exotic lists

  (cons 1 (cons 2 (cons 3 nil)))
  '(1 2 3)
  '(1 . (2 . (3 . nil)))

  ;; representations of lists above are equivalents in its implementation
  ;; just conses of cells.

  ;; association lists (dotted lists)
  (assoc 'lisa *drink-order*)
  (push '(lisa . large-mocha-with-whipped-cream) *drink-order*)
  (assoc 'lisa *drink-order*)

  ;; circular lists
  ;; (let ((foo '(1 2 3)))
  ;;   (setf (cdddr foo) foo)) ;; circle list!!

  ;; substitute-if higher-order function
  (substitute-if 0 #'oddp '(1 2 3 4 5 6 7 8 9 10))
  ;; => (0 2 0 4 0 6 0 8 0 10)

  ;; complement higher-order function
  ;; (complement #'oddp) <=> (lambda (x) (not (oddp x)))

  (nodes->dot *wizard-nodes*)
  ;; => LIVING_ROOM[label="(LIVING-ROOM (YOU ARE IN TH..."];
  ;; => GARDEN[label="(GARDEN (YOU ARE IN A BEATI..."];
  ;; => ATTIC[label="(ATTIC (YOU ARE IN THE ATTI..."];

  (dot-label (expt 10 35))
  ;; => "100000000000000000000000000..."
  (subseq '(1 2 3 4) 0 2)
  ;; => (1 2)
  (graph->dot *wizard-nodes* *wizard-edges*)
  ;; =>
  ;; digraph{
  ;; LIVING_ROOM[label="(LIVING-ROOM (YOU ARE IN TH..."];
  ;; GARDEN[label="(GARDEN (YOU ARE IN A BEATI..."];
  ;; ATTIC[label="(ATTIC (YOU ARE IN THE ATTI..."];
  ;; LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
  ;; LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
  ;; GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
  ;; ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];}
  (edges->dot *wizard-edges*)
  ;; => LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
  ;; => LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
  ;; => GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
  ;; => ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];

  ;; writes "Hello File!" into "testfile.txt"
  (with-open-file (my-stream
                   "testfile.txt"
                   :direction :output ;; ??
                   :if-exists :supersede) ;; ?!?!?
    (princ "Hello File!" my-stream))
  ;; :direction :output => we're only writing to the file and not reading it
  ;; :if-exists :supersede => if a file by that name already exists, just too out the old version


  (graph->png "wizard-graph.dot" *wizard-nodes* *wizard-edges*)
  (ugraph->png "wizard-graph-undirected.dot"
               *wizard-nodes*
               *wizard-edges*)

  ;; wow, this works! GREAT.

  ;; maplist iterating by cdr
  ;; maplist itearting by car
  ;; map needs a selector
  (mapcar #'print '(a b c))
  ;; =>
  ;; A
  ;; B
  ;; C

  (maplist #'print '(a b c))
  ;; =>
  ;; (A B C)
  ;; (B C)
  ;; (C)
  )
;; EOF
