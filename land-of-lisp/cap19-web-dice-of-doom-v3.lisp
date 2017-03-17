;; Common Lisp Script
;; Manoel Vilela

(load "cap13-webserver") ;; as :webserver package
(load "cap17-domain-specific-languages") ;; as :svg package
(load "cap18-dice-of-doom-v2") ;; as :dice-of-doom-v2 package
(load "cap18-lazy-programming") ;; as :lazy package

(defpackage :dice-of-doom-v3
  (:use :cl :svg :dice-of-doom-v2 :lazy :webserver)
  (:export :main
           :gen-board
           :attacking-moves
           :handle-human
           :handle-computer
           :get-ratings
           :limit-tree-depth
           :*num-players*
           :*die-colors*
           :*ai-level*
           :*board-width* :*board-height*
           :*board-scale* :*top-offset*
           :*dice-scale*  :*dot-size*))

(in-package :dice-of-doom-v3)


(defparameter *board-width* 900)
(defparameter *board-height* 500)
(defparameter *board-scale* 64)
(defparameter *top-offset* 3)
(defparameter *dice-scale* 40)
(defparameter *dot-size* 0.05)
(defparameter *die-colors* '((255 63 63) (63 63 255)))
(defparameter *cur-game-tree* nil)
(defparameter *from-tile* nil)

(defun draw-die-svg (x y col)
  (labels ((calc-pt (pt)
             (cons (+ x (* *dice-scale* (car pt)))
                   (+ y (* *dice-scale* (cdr pt)))))
           (f (pol col)
             (polygon (mapcar #'calc-pt pol) col)))
    (f '((0 . -1) (-0.6 . -0.75) (0 . -0.5) (0.6 . -0.75))
       (brightness col 40))
    (f '((0 . -0.5) (-0.6 . -0.75) (-0.6 . 0) (0 . 0.25))
       col)
    (f '((0 . -0.5) (0.6 . -0.75) (0.6 . 0) (0 . 0.25))
       (brightness col -40))
    (mapc (lambda (x y)
            (polygon (mapcar (lambda (xx yy)
                               (calc-pt (cons (+ x (* xx *dot-size*))
                                              (+ y (* yy *dot-size*)))))
                             '(-1 -1 1 1)
                             '(-1 1 1 -1))
                     '(255 255 255)))
          '(-0.05 0.125 0.3 -0.3 -0.125 0.05 0.2 0.2 0.45 0.45 -0.45 -0.2)
          '(-0.875 -0.80 -0.725 -0.775 -0.70 -0.625
            -0.35 -0.05 -0.45 -0.15 -0.45 -0.05))))

(defun draw-tile-svg (x y pos hex xx yy col chosen-tile)
  (loop for z below 2
        do (polygon (mapcar (lambda (pt)
                              (cons (+ xx (* *board-scale* (car pt)))
                                    (+ yy (* *board-scale*
                                             (+ (cdr pt) (* (- 1 z) 0.1))))))

                            '((-1 . -0.2) (0 . -0.5) (1 . -0.2)
                              (1 . 0.2) (0 . 0.5) (-1 . 0.2)))
                    (if (eql pos chosen-tile)
                        (brightness col 100)
                        col)))
  (loop for z below (second hex)
        do (draw-die-svg (+ xx
                            (* *dice-scale*
                               0.3
                               (if (oddp (+ x y z))
                                   -0.3
                                   0.3)))
                         (- yy (* *dice-scale* z 0.8)) col)))

(defun make-game-link (pos)
  (format nil "/game.html?chosen=~a" pos))


(defun draw-board-svg (board chosen-tile legal-tiles)
  (loop for y below *board-size*
        do (loop for x below *board-size*
                 for pos = (+ x (* *board-size* y))
                 for hex = (aref board pos)
                 for xx = (* *board-scale* (+ (* 2 x) (- *board-size* y)))
                 for yy = (* *board-scale* (+ (* y 0.7) *top-offset*))
                 for col = (brightness (nth (first hex) *die-colors*)
                                       (* -15 (- *board-size* y)))
                 do (if (member pos legal-tiles)
                        (tag g ()
                          (tag a ("xlink:href" (make-game-link pos))
                            (draw-tile-svg x y pos hex xx yy col chosen-tile)))
                        (draw-tile-svg x y pos hex xx yy col chosen-tile)))))


(defun web-initialize ()
  (setf *from-tile* nil)
  (setf *cur-game-tree* (game-tree (gen-board) 0 0 t)))

(defun web-announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (car w)))))
  (tag a (href "game.html")
    (princ " play again")))

(defun web-handle-human (pos)
   (cond ((not pos) (princ "Please choose a hex to move from:"))
        ((eq pos 'pass) (setf *cur-game-tree*
                              (cadr (lazy-car (caddr *cur-game-tree*))))
          (princ "Your reinforcements have been placed.")
         (tag a (href (make-game-link nil))
              (princ "continue")))
         ((not *from-tile*) (setf *from-tile* pos)
                           (princ "Now choose a destination:"))
         ((eq pos *from-tile*) (setf *from-tile* nil)
                              (princ "Move cancelled."))
         (t (setf *cur-game-tree*
                 (cadr (lazy-find-if (lambda (move)
                                       (equal (car move)
                                              (list *from-tile* pos)))
                                     (caddr *cur-game-tree*))))
           (setf *from-tile* nil)
           (princ "You may now ")
            (tag a (href (make-game-link 'pass))
                (princ "pass"))
            (princ " or make another move:"))))

(defun web-handle-computer ()
  (setf *cur-game-tree* (handle-computer *cur-game-tree*))
  (princ "The computer has moved. ")
  (tag script ()
    (princ "window.setTimeout('window.location=\"game.html?chosen=NIL\"',5000)")))

(defun draw-dod-page (tree selected-tile)
  (svg *board-width*
    *board-height*
    (draw-board-svg (cadr tree)
                    selected-tile
                    (take-all (if selected-tile
                                  (lazy-mapcar
                                   (lambda (move)
                                     (when (eql (caar move)
                                                selected-tile)
                                       (cadar move)))
                                   (caddr tree))
                                  (lazy-mapcar #'caar (caddr tree)))))))



(defun dod-request-handler (path header params)
  (declare (ignore header))
  (if (equal path "game.html")
      (progn (html
              (body
               (tag center ()
                    (princ "Welcome to DICE OF DOOM!")
                    (tag br ())
                    (let ((chosen (assoc 'chosen params)))
                      (when (or (not *cur-game-tree*)
                                (not chosen))
                        (setf chosen nil)
                        (web-initialize))
                      (cond ((lazy-null (caddr *cur-game-tree*))
                             (web-announce-winner (cadr *cur-game-tree*)))
                            ((zerop (car *cur-game-tree*))
                             (web-handle-human
                              (when chosen
                                (read-from-string (cdr chosen)))))
                            (t (web-handle-computer))))
                    (tag br ())
                    (draw-dod-page *cur-game-tree* *from-tile*)))))
      (princ "Sorry... I don't know that page.")))


(defun main ()
  (serve #'dod-request-handler))


;; NOTE:
;; UNFORTUNATELY THIS EXAMPLE DOESN'T WORKS ON MODERN BROWSERS
;; BECAUSE THE BAD IMPLEMENTATION OF :WEBSERVER WITHOUT SEND A STANDARD HEADER.

;; By the way, the svg drawing is fucked up too. GREAT.
