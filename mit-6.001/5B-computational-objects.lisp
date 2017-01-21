;; Common Lisp Script
;; Manoel Vilela

;; digital circuit builder
;; computational objects using state and assignment (previous lecture)
;;
;;
;;     |\
;; ----| *--- (inverter)
;;     |/
;;     ____
;; ---|    \
;;    |     |-- (and-gate)
;; ---|____/
;; _______
;;    \   \
;;     |   |-- (or-gate)
;; ___/___/
;;

;; agenda primitives
;; (make-agenda)
;; (current-time agenda)
;; (empty-agenda? agenda)
;; (add-to-agenda! time action agenda)
;; (first-item agenda)
;; (remove-first-item agenda)

;; implementing the agenda is George problem.

(defparameter the-agenda (make-agenda))
(defparameter inverter-delay 2)
(defparameter and-gate-delay 3)
(defparameter or-gate-delay 5)

(defun after-delay (delay action)
  (add-to-agenda!
   (+ delay (current-time the-agenda))
   action
   the-agenda))

(defun propagate ()
  (cond ((empty-agenda? the-agenda) 'done)
        (t (progn (funcall (first-item the-agenda))
                  (remove-first-item! the-agenda)
                  (propagate)))))

(defun call-each (procedures)
  (cond ((null procedures) 'done)
        (t (progn (funcall (car procedures))
                  (call-each (cdr procedures))))))

(defun get-signal (wire)
  (wire 'get-signal))

(defun set-signal! (wire new-value)
  (apply (wire 'set-signal!) '(new-value)))

(defun add-action! (wire action-proc)
  (apply (wire 'add-action!) '(action-proc)))

(defun make-wire ()
  (let ((signal 0)
        (action-procs '()))
    (labels ((set-my-signal! (new)
               (cond ((= signal new) 'done)
                     (t (progn (setq signal new)
                               (call-each action-procs)))))
             (accept-action-proc (proc)
               (setq action-procs (cons proc action-procs))
               (proc))
             (dispatch (m)
               (cond ((eq m 'get-signal) signal)
                     ((eq m 'set-signal) set-my-signal!)
                     ((eq m 'add-action)
                      accept=action-proc)
                     (t (error "Bad message" m)))))
      dispatch)))

(defun error (&rest messages)
  (loop for m in messages do (princ m)))

(defun logical-not (s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (t (error *invalid-signal* s))))

(defun logical-and (s1 s2)
  (cond ((and (eq s1 1)
              (eq s2 1)) 1)
        (t 0)))

(defun logical-or (s1 s2)
  (cond ((or (eq s1 1)
             (eq s2 1)) 1)
        (t 0)))


(defun inverter (in out)
  (labels ((inverter-in ()
             (let ((new (logical-not (get-signal in))))
               (after-dealy inverter-delay (lambda () (set-signal out new))))))
    (add-action! in invert-in)))

(defun and-gate (a1 a2 output)
  (labels ((and-action-procedure ()
             (let ((new-value (logical-and (get-signal a1)
                                           (get-signal a2))))
               (after-delay and-gate-delay
                            (lambda ()
                              (set-signal! output
                                           new-value))))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)))

(defun or-gate (a1 a2 output)
  (labels ((or-action-procedure ()
             (let ((new-value (logical-or (get-signal a1)
                                           (get-signal a2))))
               (after-delay or-gate-delay
                            (lambda ()
                              (set-signal! output
                                           new-value))))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)))

(defparameter a (make-wire))
(defparameter b (make-wire))
(defparameter c (make-wire))
(defparameter d (make-wire))
(defparameter e (make-wire))
(defparameter s (make-wire))

(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)

(defun half-adder (a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)))

(defun full-adder (a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))


;; Bonus => Lambda Calculus Mutable Data
;; Redefining Cons with only lambdas

(defun cons-lambda (x y)
  (lambda (m)
    (apply m '(x
               y
               (lambda (n) (setq x y))
               (lambda (n) (setq y n))))))

(defun car-lambda (c)
  (apply c (list (lambda (a d sa sd) a))))

(defun cdr-lambda (c)
  (apply c (list (lambda (a d sa sd) d))))

(defun set-car! (c n)
  (apply c (list (lambda (a d sa sd) (sa n)))))

(defun set-cdr! (c n)
  (apply c (list (lambda (a d sa sd) (sd n)))))
