;; Common Lisp Script
;; Manoel Vilela


#|

  -- ORC BATTLE GAME --

In the Orc Battle game, you are a knight surrounded by 12 monsters, engaged
in a fight to the death. With your superior wits and your repertoire
of sword-fighting maneuvers, you must carefully make strategies in your battle with orcs,
hydras, and other nasty enemies.


Using DEFMETHOD and DEFDESTRUCT, let's dispatch some whoop ass on these vermin!

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
(defparameter *monster-num* 12) ;; high => more difficult

;; undefined functions yet:
;; +MONSTER-HIT
;; +MONSTERS-DEAD
;; +PICK-MONSTER
;; +RANDVAL
;; I need define the structure of MONSTER as well yet.

(defun init-player()
  "Set the initial tributes of our knight"
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  "Check if the player is alive"
  (<= *player-health* 0))

(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defun show-player ()
  "If the player is alive, show in REPL your info at each action"
  (fresh-line)
  (format t "You are a valiant knight with a health of ~a,
             an agility of ~a and a strength of ~a"
          *player-health*
          *player-agility*
          *player-health*))

(defun randval (n)
  (1+ (random (max 1 n))))

;; helper functions for player attack
(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
        (random-monster)
        m)))

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  (let ((x read))
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

;; monster management functions
(defun init-monsters ()
  (setf *monsters*
        (map 'vector
             (lambda (x)
               (funcall (nth (random (length *monsters-builders*)))
                        *monster-builders*))
             (make-array *monster-num*))))

(defun player-attack ()
  "The player-attack function lets us manage a player's attack"
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse: ")
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
                   (monster-hit (random-monster 1)))))))



(defun game-loop ()
  "The game-loop function handles the repeated cycles of monster
   and player attacks."
  (unless (or (player-dead)
              (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda (m)
           (or (monster-dead m)
               (moster-attack m)))
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

