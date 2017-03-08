;; Common Lisp Script
;; Manoel Vilela

(ql:quickload 'usocket)

;; NOTE: in that example i need to force the stream with (finish-output)
;; to effectively send messages between sockets.

;; listening socket
(defparameter *my-socket* (usocket:socket-listen "127.0.0.1" 1234))

(princ "SERVER: Waiting client connection... ")
(finish-output)
(defparameter *socket-connection* (usocket:socket-accept *my-socket*))
(defparameter *socket-stream* (usocket:socket-stream *socket-connection*))
(format t "Connected! ~%")
;; after running this command, the server will seem to lock up,
;; and you won't be returned to the REPL prompt.
;; Don't be alarmed, the socket-accept command is a blocking operation,
;; which means the function won't exit until a client has connected.

(format t "<from-client>: ~a ~%" (read-line *socket-stream*))
(write-line "What's up, Client!" *socket-stream*)
(finish-output *socket-stream*) ;; flush buffered stream
(usocket:socket-close *my-socket*)
