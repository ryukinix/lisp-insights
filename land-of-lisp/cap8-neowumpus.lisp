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


;; create edges for isolated nodes (islands)
(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

;; create edges for all isolated nodes
(defun connect-all-islands (node edge-list)
  (append (connect-with-bridges (find-islands node edge-list))
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
  (let* ((nodes (loop for i from 1 to *node-num*))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x)
                                (declare (ignore x)) ;; sbcl stuff, avoid warning
                                (zerop (random *cop-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

;; selectors

(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))


