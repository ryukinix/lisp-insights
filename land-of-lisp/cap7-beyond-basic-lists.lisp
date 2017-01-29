;; Common Lisp Script
;; Manoel Vilela

;; exotic lists

(cons 1 (cons 2 (cons 3 nil)))
'(1 2 3)
'(1 . (2 . (3 . nil)))

;; representations of lists above are equivalents in its implementation
;; just conses of cells.


;; circular lists


(defparameter foo '(1 2 3))
(setf (cdddr foo) foo) ;; circle list!!!


;; associative lists

(defparameter *drink-order* '((bill . double-espresso)
                              (lisa . samll-drip-coffee)
                              (john . medium-latte)))

(assoc 'lisa *drink-order*)
(push '(lisa . large-mocha-with-whipped-cream) *drink-order*)
(assoc 'lisa *drink-order*)


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
