;; Common Lisp Script
;; Manoel Vilela

(defpackage :dice-of-doom-v4
  (:use :cl :dice-of-doom-v3))

(in-package :dice-of-doom-v4)


;; YES, MORE PLAYERS
(defparameter *num-players* 4)
(defparameter *die-colors* '((255 63 63) (63 63 255) (63 255 63)
                             (255 63 255)))
(defparameter *max-dice* 5)
(defparameter *ai-level* 2)

#|

LISP OVERHEAT

I'm tired of this shit example of DICE OF DOOM! THIS IS NOT FUN.

Go to hell web-based games.

|#

;; NOTE: Skipping this chapter. I'll note write a new version of Dice of Doom! Holy crap,
;; four versions of a stupid game?! NOOOO!. I will do something more useful.

;; BTW, this shit style to redefining functions and using package of earlier versions is
;; a absolutely hell. If you wanna write something great, start writing the great thing first.
;; REDEFINING AND MIXING-UP IS EVIL.
