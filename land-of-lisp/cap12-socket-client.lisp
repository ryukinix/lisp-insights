;; Common Lisp Script
;; Manoel Vilela

(ql:quickload 'usocket)

(princ "CLIENT: Trying connect to server... ")
(defparameter *socket-connection* (usocket:socket-connect "127.0.0.1" 1234))
(defparameter *socket-stream* (usocket:socket-stream *socket-connection*))
(format t "Connected! ~%")

(write-line "Yo Server!" *socket-stream*)
(finish-output *socket-stream*) ;; force send messaging
(format t "<from-server>: ~a ~%" (read-line *socket-stream*))

(usocket:socket-close *socket-connection*)
