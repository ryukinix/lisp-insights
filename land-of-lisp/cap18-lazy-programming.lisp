;; Common Lisp Script
;; Manoel Vilela

;; LAZY PROGRAMMING

;; PROG: Creating the Lazy and Force Commands
;; NOTE: This chapter has a lazy evaluation system very similar
;;       to discussed on the lectures about STREAMS (6A-6B) of SICP at MIT.

(defmacro lazy (&body body)
  (let ((forced (gensym))
        (value (gensym)))
    `(let ((,forced nil)
           (,value nil))
       (lambda ()
         (unless ,forced
           (setf ,value (progn ,@body))
           (setf ,forced t))
         ,value))))

(defun force (lazy-value)
  (funcall lazy-value))


(defmacro lazy-cons (a b)
  `(lazy (cons ,a ,b)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

(defparameter *foo* (lazy-cons 4 7)) ;; => CLOSURE LAMBDA
(lazy-car *foo*) ;; => 4
(lazy-cdr *foo*) ;; => 7


(defparameter *integers* (labels ((f (n)
                                    (lazy-cons n (f (1+ n)))))
                           (f 1)))

(lazy-car *integers*) ;; => 1
(lazy-car (lazy-cdr *integers*)) ;; => 2
(lazy-car (lazy-cdr (lazy-cdr *integers*))) ;; => 3

;; YES! INFINITE SEQUENCES! This is the lazy evaluation power
;; Only computes the value when needs. No recursive stack overflow.


(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))


(defun make-lazy (list)
  (lazy (when list
          (cons (car list)
                (make-lazy (cdr list))))))


(defun take (n list)
  (unless (or (zerop n)
              (lazy-null list))
    (cons (lazy-car list)
          (take (1- n) (lazy-cdr list)))))

(take 100 *integers*) ;; => '(1 2 3 4 5 6 7 8 9 10)

(defun take-all (list)
  (unless (lazy-null list)
    (cons (lazy-car list)
          (take-all (lazy-cdr list)))))

(take 10 (make-lazy '(q w e r t y u i o p a s d f)))
;; => (Q W E R T Y U I O P)

(take-all (make-lazy '(q w e r t y u i o p a s d f)))
;; => (Q W E R T Y U I O P A S D F)


(defun lazy-mapcar (fun list)
  (lazy (unless (lazy-null list)
          (cons (funcall fun (lazy-car list))
                (lazy-mapcar fun (lazy-cdr list))))))

(defun lazy-mapcan (fun list)
  (labels ((f (list-cur)
             (if (lazy-null list-cur)
                 (force (lazy-mapcan fun (lazy-cdr list)))
                 (cons (lazy-car list-cur)
                       (lazy (f (lazy-cdr list-cur)))))))
    (lazy (unless (lazy-null list)
            (f (funcall fun (lazy-car list)))))))

(defun lazy-find-if (fun list)
  (unless (lazy-null list)
    (let ((x (lazy-car list)))
      (if (funcall fun x)
          x
          (lazy-find-if fun (lazy-cdr list))))))

(defun lazy-nth (n list)
  (if (zerop n)
      (lazy-car list)
      (lazy-nth (1- n) (lazy-cdr list))))


;; NOTE: Analogous functions mapcar, mapcan, find-if and nth for lazy lists.
(take 10 (lazy-mapcar #'sqrt *integers*))
;; => (1.0 1.4142135 1.7320508 2.0 2.236068 2.4494898 2.6457512 2.828427 3.0 3.1622777)

(take 10 (lazy-mapcan (lambda (x)
                        (if (evenp x)
                            (make-lazy (list x))
                            (lazy-nil)))
                      *integers*))
;; => (2 4 6 8 10 12 14 16 18 20)

(lazy-find-if #'oddp (make-lazy '(2 4 6 7 8 10)))
;; => 7

(lazy-nth 4 (make-lazy '(a b c d e f g)))
;; => E
