;; Common Lisp Script
;; Manoel Vilela


#| Dice of Doom :: A Game Written in The Functional Style |#


#| THE RULES

* Two players (named A and B) occupy spaces on a hexagonal grid.
  Each hexagon in the grid will have some six-sided dice on it, owned
  by the occupant

* During a turn, a player can perform any number of moves, but must
  perform at least one move. If the player cannot move, the game ends.

* A move consists of attacking a neighboring hexagon owned by the
  opponent. The player must have more dice in her hexagon than the
  neighboring hexagon in order to attack. For now, all attacks will
  automatically lead to a win. In future variants, we'll actually roll
  the dice for a battle. But for now, the player with more dice just wins.

* After winning a battle, the losing player's dice are removed from the board,
  and all but one of the winning player's dice are moved onto the newly
  won hexagon

* After a player is finished making her moves, reinforcements are added to
  that player's dice armies. Reinforcements to the player's occupied hexagons
  are added one die at a time, starting from the upper-left corner, moving
  across and down. The maximum number of dice added as reinforcements is one
  less than the player took from the opponent in her completed turn

* When a player can no longer take her turn, the game has ended.
  The player who occupies the most hexagons at this point is the winner.
  (A tie is also possible)

|#

;; Implementing Dice of Doom, Version 1

(defpackage :dice-of-doom
  (:use :cl)
  (:export :*num-players*
           :*max-dice*
           :*board-size*
           :*board-hexnum*
           :gen-board
           :add-passing-move
           :attacking-moves
           :add-new-dice
           :print-info
           :board-attack
           :add-new-dice
           :moves
           :game-tree
           :neighbors
           :announce-winner
           :get-ratings
           :rate-position
           :handle-computer
           :handle-human
           :play-vs-computer
           :play-vs-human
           :computer
           :human))

(in-package :dice-of-doom)

;; Dirty part :: Global variables
(defparameter *num-players* 2 "the number of players in-game")
(defparameter *max-dice* 3 "max of dice for each cell")
(defparameter *board-size* 3 "the board size length")
(defparameter *board-hexnum* (* *board-size* *board-size*) "number of cells")

;; Representing the Game Board
;; Alist with (player-owner n-of-dices)

;; => functional
(defun board-array (list)
  "Generate a board array based on alist with <player, n-dice>
   + TEST
   > (board-array '((1 1) (1 1) (1 2) (0 3)))
   #((1 1) (1 1) (1 2) (0 3))"
  (make-array (length list) :initial-contents list))

;; => imperative
(defun gen-board ()
  "Generate a board array with random parameters
   + TEST
   > (gen-board)
   #((1 1) (1 1) (1 2) (0 3))"
  (board-array (loop for n below *board-hexnum*
                     collect (list (random *num-players*)
                                   (1+ (random *max-dice*))))))

;; => functional
(defun player-letter (n)
  "Get a alphabet letter given a n >= 0
   + TEST
   > (player-letter 0)
   #\a
   > (player-letter 1)
   #\b"
  (code-char (+ 97 n)))

;; => imperative
(defun draw-board (board)
  "Draw the board in a nice way to visualize the dices on
   each cell of players.
   + TEST
   > (draw-board (gen-board))
      a-3 a-3
    a-3 a-2"
  (loop for y below *board-size*
        do (progn (fresh-line)
                  (loop repeat (- *board-size* y)
                        do (princ "  "))
                  (loop for x below *board-size*
                        for hex = (aref board (+ x (* *board-size* y)))
                        do (format t "~a-~a " (player-letter (first hex))
                                              (second hex))))))

;; Decoupling Dice of Doom's Rules from the Rest of the Game
;; Modules:
;; + Human player moves handling
;; + AI Player
;; + Rule Engine

;; Generating a Game Tree

;; => functional
(defun game-tree (board player spare-dice first-move)
  "Generate the game tree of moves"
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))

;; Calculating Passing Moves

;; => functional
(defun add-passing-move (board player spare-dice first-move moves)
  "Add a new move to the game tree"
  (if first-move
      moves
      (cons (list nil
                  (game-tree (add-new-dice board player (1- spare-dice))
                             (mod (1+ player) *num-players*)
                             0
                             t))
            moves)))

;; undefined function:
;; + add-new-dice

;; Finding the neighbors

;; => functional
(defun neighbors (pos)
  "Given the cell position, return the neighbors
   + TEST
   > (neighbors 2)
   (0 3)"
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
          when (and (>= p 0)
                    (< p *board-hexnum*))
            collect p)))

;; Attacking
;; => functional
(defun board-attack (board player src dst dice)
  "Generate a new board after attack of player src->dst with n dice
   + TEST
   > (board-attack #((0 3) (0 3) (1 3) (1 1)) 0 1 3 3)
   #((0 3) (0 1) (1 3) (0 2))"
  (board-array (loop for pos from 0
                     for hex across board
                     collect (cond ((eq pos src) (list player 1))
                                   ((eq pos dst) (list player (1- dice)))
                                   (t hex)))))
;; => functional
(defun attacking-moves (board cur-player spare-dice)
  "Check attack rules"
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    (mapcan (lambda (src)
              (when (eq (player src) cur-player)
                (mapcan (lambda (dst)
                          (when (and (not (eq (player dst) cur-player))
                                     (> (dice src) (dice dst)))
                            (list (list (list src dst)
                                        (game-tree (board-attack board cur-player src dst (dice src))
                                                   cur-player
                                                   (+ spare-dice (dice dst))
                                                   nil)))))
                        (neighbors src))))
            (loop for n below *board-hexnum*
                  collect n))))

;; NOTE: Many of the functions in this chapter have inefficiencies to keep things simple.
;;       We'll fix may of these in future versions of the game. (next chapters)


;; :: Reinforcements
;; => functional
;; NOTE: OLD-VERSION, NOT TAIL RECURSIVE
;; (defun add-new-dice (board player spare-dice)
;;   "Add reinforcements at player cells on board based on spare-dice
;;    + TEST
;;    > (add-new-dice #((0 1) (1 3) (0 2) (1 1)) 0 2)
;;    #((0 2) (1 3) (0 3) (1 1))"
;;   (labels ((f (list n)
;;              (cond ((null list) nil)
;;                    ((zerop n) list)
;;                    (t (let ((cur-player (caar list))
;;                             (cur-dice (cadar list)))
;;                         (if (and (eq cur-player player)
;;                                  (< cur-dice *max-dice*))
;;                             (cons (list cur-player (1+ cur-dice))
;;                                   (f (cdr list) (1- n)))
;;                             (cons (car list) (f (cdr list) n))))))))
;;     (board-array (f (coerce board 'list) spare-dice))))



;; Tail Call Optimization

;; => re-write as tail call recursive

(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n acc)
             (cond ((zerop n) (append (reverse acc) lst))
                   ((null lst) (reverse acc))
                   (t (let ((cur-player (car lst))
                            (cur-dice (cadar lst)))
                        (if (and (eq cur-player player)
                                 (< cur-dice *max-dice*))
                            (f (cdr lst)
                               (1- n)
                               (cons (list cur-player (1+ cur-dice )) acc))
                            (f (cdr lst) n (cons (car lst) acc))))))))
    (board-array (f (coerce board 'list) spare-dice nil))))



;; => imperative
(defun print-info (tree)
  "Describes the status of current node in the game tree"
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))

;; => imperative
(defun handle-human (tree)
  "Get the human chosen move given the possibilities"
  (fresh-line)
  (princ "Choose your move: ")
  (let ((moves (caddr tree)))
    (loop for move in moves
          for n from 1
          do (let ((action (car move)))
               (fresh-line)
               (format t "~a. " n)
               (if action
                   (format t "~a -> ~a" (car action) (cadr action))
                   (princ "end turn"))))
    (fresh-line)
    (cadr (nth (1- (read)) moves))))

;; => functional
(defun winners (board)
  "Parse the board to detect if someone wins and return a list of winners"
  (let* ((tally (loop for hex across board
                      collect (car hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
            (remove-if (lambda (x)
                         (not (eq (cdr x) best)))
                       totals))))

;; => imperative
(defun announce-winner (board)
  "Print the winner or tie given the board"
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (car w))))))

;; => imperative
(defun play-vs-human (tree)
  "Start a game vs a human"
  (print-info tree)
  (if (caddr tree)
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))

;; add simple AI based on min-max algorithm


;; => functional
(defun rate-position (tree player)
  "Tag score (best move) for each branch on tree"
  (let ((moves (caddr tree)))
    (if moves
        (apply (if (eq (car tree) player)
                   #'max
                   #'min)
               (get-ratings tree player))
        (let ((w (winners (cadr tree))))
          (if (member player w)
              (/ 1 (length w))
              0)))))
;; => functional
(defun get-ratings (tree player)
  "Get the ratings for each branch"
  (mapcar (lambda (move)
            (rate-position (cadr move) player))
          (caddr tree)))

;; => imperative
(defun handle-computer (tree)
  "Handle AI moves"
  (let ((ratings (get-ratings tree (car tree))))
    (cadr (nth (position (apply #'max ratings) ratings)
               (caddr tree)))))

;; => imperative
(defun play-vs-computer (tree)
  "Play with an AI as enemy"
  (print-info tree)
  (cond ((null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))

(defun human ()
  "Start a play-vs-human game with a random board"
  (play-vs-human (game-tree (gen-board) 0 0 t)))

(defun computer ()
  "Start a play-vs-computer game with a random board"
  (play-vs-computer (game-tree (gen-board) 0 0 t)))


;; :: Optimizations for Functional Style

;; Memoization

;; neighbors
(let ((old-neighbors (symbol-function 'neighbors))
      (previous (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
        (setf (gethash pos previous)
              (funcall old-neighbors pos)))))

;; game-tree
(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
        (setf (gethash rest previous) (apply old-game-tree rest)))))


;; rate-position
(let ((old-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))
      (unless tab
        (setf tab (setf (gethash player previous) (make-hash-table))))
      (or (gethash tree tab)
          (setf (gethash tree tab)
                (funcall old-rate-position tree player))))))


;; NOTE: You use memoization for optimizing the performance of code
;; written in the functional style. However, memoization code is not,
;; in itself, written in the functional style. It cannot be, since
;; it requires you to maintain and update a table of previous calls
;; to the target function.
