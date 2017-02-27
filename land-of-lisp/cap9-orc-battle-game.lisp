;; Common Lisp Script
;; Manoel Vilela


#|

  -- ORC BATTLE GAME --

In the Orc Battle game, you are a knight surrounded by 12 monsters, engaged
in a fight to the death. With your superior wits and your repertoire
of sword-fighting maneuvers, you must carefully make strategies in your battle with orcs,
hydras, and other nasty enemies.

c
Using DEFMETHOD and DEFSTRUCT, let's dispatch some whoop ass on these vermin!

|#


;; global variables for player status

(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)


;; we'll store our monsters in an array called *monsters*
;; we'll also define a list of functions for building monsters that
;; we'll store in the variable *monster-builders* (AQUI É MONSTRO, PORRA)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 6) ;; high => more difficult

(defun randval (n)
  (1+ (random (max 1 n))))

;; STRUCTS

;; THE MONSTERS

;; generic monster strict
(defstruct monster (health (randval 10)))

;; THE WICKED ORC
(defstruct (orc (:include monster))
           (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at you knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))


;; THE MALICIOUS HYDRA
(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads. "))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "The corpse of the fully decapitated hydra falls to the floor! ")
      (progn (princ "You lop off ")
             (princ x)
             (princ " of hydra's heads! "))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)))

(defstruct (slime-mold (:include monster))
           (slimeness (randval 5)))
(push #'make-slime-mold *monster-builders*)


(defmethod monster-attack ((m slime-mold))
  (princ "A slime mold of slimness of ")
  (princ (slime-mold-slimeness m))
  (princ " attacks "))

(defmethod monster-show ((m slime-mold))
  (let ((x (randval (slime-mold-slimeness m))))
    (princ "A slime mold wraps around your legs and decreases your agility by  ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*))))

(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health*
                *player-agility*
                *player-strength*)))
    (cond ((= x *player-health*)
           (princ "A brigand hits you with his slingshot taking off 2 health points! ")
           (decf *player-health* 2))
          ((= x *player-agility*)
           (princ "A brigand catches your leg with his whip taking off 2 agility points! ")
           (decf *player-agility* 2))
          ((= x *player-strength*)
           (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
           (decf *player-strength* 2)))))


(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (princ "You killed the ")
             (princ (type-of m))
             (princ "! "))
      (progn (princ "You hit the ")
             (princ (type-of m))
             (princ ", knocking off")
             (princ x)
             (princ " health points! "))))



(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))

(defmethod monster-attack (m))

;; helper functions for player attack
(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
        (random-monster)
        m)))

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  (fresh-line)
  (let ((x (read)))
    (if (not (and (integerp x)
                  (>= x 1)
                  (<= x *monster-num*)))
        (progn (princ "That is not a valid monster number.")
               (pick-monster))
        (let ((m (aref *monsters* (1- x))))
          (if (monster-dead m)
              (progn (princ "That monster is already dead.")
                     (pick-monster))
              m)))))

(defun init-monsters ()
  (setf *monsters*
        (map 'vector
             (lambda (x)
               (declare (ignore x))
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))

(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let ((x 0))
    (map 'list
         (lambda (m)
           (fresh-line)
           (princ "   ")
           (princ (incf x))
           (princ ".  ")
           (if (monster-dead m)
               (princ "**dead**")
               (progn (princ "(Health=")
                      (princ (monster-health m))
                      (princ ") ")
                      (monster-show m))))
         *monsters*)))

(defun init-player()
  "Set the initial tributes of our knight"
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  "Check if the player is alive"
  (<= *player-health* 0))

(defun show-player ()
  "If the player is alive, show in REPL your info at each action"
  (fresh-line)
  (format t "You are a valiant knight with a health of ~a, an agility of ~a and a strength of ~a"
          *player-health*
          *player-agility*
          *player-health*))

(defun player-attack ()
  "The player-attack function lets us manage a player's attack"
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse: ")
  (fresh-line)
  (case (read)
    (s (monster-hit (pick-monster)
                    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
         (format t "Your double swing has a strength of ~a" x)
         (fresh-line)
         (monster-hit (pick-monster) x)
         (unless (monsters-dead)
           (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                 (unless (monsters-dead)
                   (monster-hit (random-monster) 1))))))



(defun game-loop ()
  "The game-loop function handles the repeated cycles of monster
   and player attacks."
  (unless (or (player-dead)
              (monsters-dead))
    (fresh-line)
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda (m)
           (or (monster-dead m)
               (monster-attack m)))
         *monsters*)
    (game-loop)))

;; the big picture function
(defun orc-battle ()
  "Main function of the game"
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes.")))



(orc-battle)
;; just execute this game via terminal as: sbcl --script this-file-name.lisp
