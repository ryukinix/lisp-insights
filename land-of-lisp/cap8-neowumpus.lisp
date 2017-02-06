;; Common Lisp Script
;; Manoel Vilela

(load "cap7-beyond-basic-lists") ;; load package on namespace :graph-until

;; functions:
;; (graph-util:ugraph->png 'fname 'nodes 'edges)
;; (graph-util:graph->png 'fname 'nodes 'edges)

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)
(defparameter *visited-nodes* nil)
(defparameter *player-pos* nil)

;; note: make sure that (random n) generates whole numbers in that range
;; => [0, n)
(defun random-node ()
  "Return a random number between 1 and *node-num* (30 for default)"
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  "Generate a edge pair given a b nodes. Return nil if a e b is equal symbols."
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  "Generate a list of edges based on random-nodes"
  (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node)
                                           (random-node)))))

(defun examples-of-loop-macro ()
  (loop repeat 10
        collect 1)
  ;; => (1 1 1 1 1 1 1 1 1 1)
  (loop for n from 1 to 10
        collect n)
  ;; => (1 2 3 4 5 6 7 8 9 10)
  (loop for n from 1 to 10
        collect (+ 100 n))
  ;; => (101 102 103 104 105 106 107 108 109 110)
  )

;; connecting isolated non-connected nodes after (make-edge-list)
;; generation using random

;; get the edges connect with node for another node
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list))

;; get the possible nodes to visit starting with this node
(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
               (unless (member node visited)
                 (push node visited)
                 ;; mapc is not destructive (didn't modify data of arguments)
                 (mapc (lambda (edge) ;;  mapc is like a for-each, but return the second argument instead nil
                         (traverse (cdr edge)))
                       (direct-edges node edge-list)))))
      (traverse node))
    visited))

;; find islands~! unconnected nodes on the edges
(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
               (let* ((connected (get-connected (car nodes) edge-list))
                      (unconnected (set-difference nodes connected))) ;; difference between two lists A - B
                 (push connected islands)
                 (when unconnected
                   (find-island unconnected)))))
      (find-island nodes))
    islands))


;; create edges for isolated group of nodes (islands)
(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

;; create edges for all isolated group of nodes
(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list))
          edge-list))


(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                    (list node2 'cops)
                                    edge)))
                            node1-edges))))
          edge-alist))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list)
                                             :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))


(defun  make-city-edges()
  (let* ((nodes (loop for i from 1 to *node-num* collect i))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x)
                                (declare (ignore x)) ;; SBCL stuff, avoid warning
                                (zerop (random *cop-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

;; selectors

(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-list)
  (or (within-one a b edge-list)
      (some (lambda (x)
              (within-one x b edge-list))
            (neighbors a edge-list))))

(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
                          collect (random-node))))
    (loop for n from 1 to *node-num*
          collect (append (list n)
                          (cond ((eql n wumpus) '(wumpus))
                                ((within-two n wumpus edge-alist) '(blood!)))
                          (cond ((member n glow-worms)
                                 '(glow-worm))
                                ((some (lambda (worm)
                                         (within-one n worm edge-alist))
                                       glow-worms)
                                '(lights!)))
                          (when (some #'cdr (cdr (assoc n edge-alist)))
                            '(sirens!))))))

(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
        (find-empty-node)
        x)))

(defun draw-city ()
  (graph-util:ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))


;; BUG FOUND HERE!!!
(defun known-city-nodes ()
  (mapcar (lambda (node)
            (if (member node *visited-nodes*)
                (let ((n (assoc node *congestion-city-nodes*)))
                  (if (eql node *player-pos*)
                      (append n '(*))
                      n))
                (list node '? )))
          (remove-duplicates
           (append *visited-nodes*
                   (mapcan (lambda (node)
                             (mapcar #'car (cdr (assoc node *congestion-city-edges*))))
                           *visited-nodes*)))))


(defun known-city-edges ()
  (mapcar (lambda (node)
            (cons node (mapcar (lambda (x)
                                 (if (member (car x) *visited-nodes*)
                                     x
                                     (list (car x))))
                               (cdr (assoc node *congestion-city-edges*)))))
          *visited-nodes*))

(defun example-mapcan-function ()
  (labels ((ingredients (order)
             (mapcan (lambda (burger)
                       (case burger
                         (single '(patty))
                         (double '(patty patty))
                         (double-cheese '(patty patty cheese))))
                     order)))
    (ingredients '(single double-cheese double))))
;; => (PATTY PATTY PATTY CHEESE PATTY PATTY)


(defun draw-known-city ()
  (graph-util:ugraph->png "known-city" (known-city-nodes) (known-city-edges)))


(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

;; some bug was tracked here
;; EDITED: actually is in known-city-nodes
;; backtrace: handle-new-place -> draw-known-city -> known-city-nodes
(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
         (has-worm (and (member 'glow-worm node)
                        (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game Over!"))
          ((member 'wumpus node) (if charging
                                     (princ "You found the Wumpus!")
                                     (princ "You ran into the Wumpus")))
          (charging (princ "You wasted your last bullet. Game Over!"))
          (has-worm (let ((new-pos (random-node)))
                       (princ "You ran into a Glow Worm Gang! You're now at ")
                       (princ new-pos)
                       (handle-new-place nil new-pos nil))))))

(defun handle-direction (pos charging)
  (let ((edge (assoc pos
                     (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
        (handle-new-place edge pos charging)
        (princ "That location does not exist!"))))

(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

;; HOW TO PLAY::
;; (load "this-file")
;; (new-game)
;; => created a file called known-city.png
;; => so well the, for spoilers, an overall map of the city is created as city.png
;; open it in a browser, it's our map
;; use (walk num) & (charge num) to walk and shot between the nodes from edges
;; at each walk/charge call a new known-city.png is generated (updated)
;; you have just one shot, so make sure to not waste this bullet
