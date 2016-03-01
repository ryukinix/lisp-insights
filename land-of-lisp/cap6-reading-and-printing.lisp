;; Common Lisp Script
;; Manoel Vilela

;; fundamentals printing

;; :: print
;; print send the arg to sdout with #\newline before the sentence
;; and add on the end a white-space too LOL something like that:
;; (format t "~% ~s " 'simbol) ;; realy crazy!!!!
;; and return the output
(print 'abacate)
(print '(some data))
(print ':this-is-a-keyword)
(print "that is a string")

;; :: prin1
;; same thing of the 'print' function, but return a space
;; instead newline
(progn (prin1 "1")
       (prin1 "2")
       (prin1 "3"))

;; simple example combining things
;; remember: read sucks
;; read operator only reads a single token.
;; if you type two tokens will break
(defun say-hello ()
  (print "Please type your name: ")
  (let ((name (read)))
    (prin1 "Nice to meet you, ")
    (prin1 name)))
(say-hello)

;; but print and prin1 is repl stuff, don't cool for humans
;; print strings with quotes and had a print esoteric behavior of newline-content-space
;; instead use that, use princ! and read-line

(progn (princ "An weird way to")
       (princ #\newline)
       (princ "To split a phrase"))

(defun say-hello ()q
  (princ "Please type your name: ")
  (let ((name (read-line))) ;; read-line is nice!
    (princ "Nice to meet you, ")
    (princ name)))

;; other examples for princ
(princ :foo)
(princ 1.2)
(princ "object")
(princ 2/3)
(princ 'symbol)
