;; Common Lisp Script
;; Manoel Vilela

;; On this chapter we'll create a SVG handler DSL using macros
;; Remember SVG (Scalable Vector Graphics) are XML-like,


(load "cap5-building-a-text-game-engine")

;; Would be better avoid copy-paste functions,
;; but the chapter 16 has several re-definitions
;; and top-level execution of demonstrations
;; So i just adapted the +SPLIT and +PAIRS functions

(defmacro split (val yes no)
  "Exported from the chapter 16"
  (let ((g (gensym)))
        `(let ((,g ,val))
               (if ,g
                   (let ((head (car ,g))
                         (tail (cdr ,g)))
                     ,yes)
                   ,no))))


(defun pairs (lst)
  "Exported from the chapter 16"
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail)
                           (cons (cons head (car tail))
                                 acc))
                        (reverse acc))
                    (reverse acc))))
    (f lst nil)))

;; so we need create tags
(defun print-tag (name alist closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\""
                  (string-downcase (car att))
                  (cdr att)))
        alist)
  (princ #\>))


(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x)
                                       `(cons ',(car x)
                                              ,(cdr x)))
                                     (pairs atts)))
                     nil)
          ,@body
          (print-tag ',name nil t)))

;; using syntax-sugar for html

(defmacro html (&body body)
  `(tag html ()
     ,@body))

(defmacro body (&body body)
  `(tag body ()
        ,@body))

(defmacro svg (&body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
                   "xmlns:xlink" "http://www.w3.org/1999/xlink")
     ,@body))

(defun brightness (col amt)
  (mapcar (lambda (x)
            (min 255 (max 0 (+ x amt))))
          col))

(defun svg-style (color)
  (format nil
          "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
          (append color
                  (brightness color -100))))

(defun circle (center radius color)
  (tag circle (cx (car center)
               cy (cdr center)
               r radius
               style (svg-style color))))

(defun polygon (points color)
  (tag polygon (points (format nil
                               "~{~a, ~a ~}"
                               (mapcan (lambda (tp)
                                         (list (car tp) (cdr tp)))
                                       points))
                style (svg-style color))))


(defun random-walk (value length)
  (unless (zerop length)
    (cons value
          (random-walk (if (zerop (random 2))
                           (1- value)
                           (1+ value))
                       (1- length)))))


;; create random polygons at random_walk.svg

(defun random-walk-svg ()
  (with-open-file (*standard-output* "random_walk.svg"
                                     :direction :output
                                     :if-exists :supersede)
    (svg (loop repeat 10
               do (polygon (append '((0 . 200))
                                   (loop for x from 0
                                         for y in (random-walk 100 400)
                                         collect (cons x y))
                                   '((400 . 200)))
                           (loop repeat 3
                                 collect (random 256)))))))


;; svg finished

;; starts extension of the wizard_game from chapter 5-6

(defun have (object)
  (member object (inventory)))

(defparameter *chain-welded* nil)

(defun weld (subject object)
  (if (and (eq *location* 'attic)
           (eq subject 'chain)
           (eq object 'bucket)
           (have 'chain)
           (have 'bucket)
           (not *chain-welded*))
      (progn (setf *chain-welded* t)
             '(the chain is now securely welded to the bucket.))
      '(you cannot weld like that.)))

(pushnew 'weld *allowed-commands*)
(defparameter *bucket-filled* nil)

(defun dunk (subject object)
  (if (and (eq *location* 'garden)
           (eq subject 'bucket)
           (eq object 'well)
           (have 'bucket)
           *chain-welded*)
      (progn (setf *bucket-filled* t)
             '(the bucket is now full of water))
      '(you cannot dunk like that)))
(pushnew 'dunk *allowed-commands*)


;; super cool macro to avoid replication like the commands above
(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj))
                ,@body
                '(i cant ,command like that.)))
          (pushnew ',command *allowed-commands*)))

(game-action splash bucket wizard living-room
  (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
        ((have 'frog) '(the wizard awakens and sees that you stole his frog.
                        he is so upset he banishes you to the netherworlds-
                        you lose! the end.))
        (t '(the wizard awakens from his slumber and greets your warmly.
             he hands you the magic low-carb donut- you win! the end.))))
