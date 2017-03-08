;; Common Lisp Script
;; Manoel Vilela

#|
 :: => STREAMS

Streams are data types in Common Lisp that allow you
to take some external resource and make it look like
just another simple piece of data you can manipulate with
your code.

Types of streams:
* file stream:
  Let us read and write to files on our hard drive
* string stream:
  Let us send and receive text from a Lisp string
* console stream:
  REPL and terminals
* socket stream:
  Let us communicate with other computers on a network

Streams by direction:
* output stream
* input stream
|#


;; Output Streams:
;; + Check whether the stream is valid
;; + Push a new item onto the stream

;; check if stream is valid
(output-stream-p *standard-output*)
;; => T

;; push a new item on stream
(write-char #\x *standard-output*)
;; STDOUT => x
;; => #\x


;; Input Streams
;; + check whether the stream is valid.
;; + pop an item off of the stream

;; check if the stream is valid
(input-stream-p *standard-input*)
;; => T

;; pop item from stream
(read-char *standard-input*)
;; INPUT => 123
;; => #\1

;; NOTE: Using other commands to interact with streams
(print 'foo *standard-output*)


;; :: Working With Files

;; write on file
(with-open-file (my-stream "data.txt" :direction :output)
  (print "my data" my-stream))
;; => "my data"
;; a new file called data.txt is created


;; read a file
(with-open-file (my-stream "data.txt" :direction :input)
  (read my-stream))
;; => "my data"


;; another example
(let ((animal-noises '((dog . woof)
                       (cat . meow))))
      (with-open-file (my-stream "animal-noises.txt" :direction :output)
        (print animal-noises my-stream)))
;; => ((DOG . WOOF) (CAT . MEOW))

(with-open-file (my-stream "animal-noises.txt" :direction :input)
  (read my-stream))
;; => ((DOG . WOOF) (CAT . MEOW))

;; using keywords of with-open-file form to throw errors
;; when a file already exists on direction output
(with-open-file (my-stream "data.txt" :direction :output
                                      :if-exists :error)
  (print "my data" my-stream))
;; *** - OPEN: file #P"/home/.../data.txt" already exists

;; use :supersede to force overwritten when :if-exists is true
(with-open-file (my-stream "data.txt" :direction :output
                                      :if-exists :supersede)
  (print "my data" my-stream))
;; => "my data"

;; NOTE:
;; The with-open-file macro is very similar to the context-manager of python
;; which is created using the `with` keyword on this language.
;; As well implement on Python, the with-open-file already cares about open
;; and closing the files gracefully. That way we don't need worry about that.
;; On Common Lisp, in general, all the `with-' commands using this prefix
;; will safely allocate resources in this way.


;; :: Working with Sockets

;; A socket is a mechanism for routing data over a computer network
;; between programs running on different computers on that network.
;; Sockets are not in the ANSI Common Lisp standard, which means
;; there's no standard way of interacting with sockets at this time.

;; libs: :cl-sockets or :usocket
;; cl-sockets is not in quicklisp, but usocket are.
;; in this section I'll use the usocket
;; https://github.com/usocket/usocket
;; running through SBCL
;; (ql:quickload 'usocket)

;; Every socket within a network must have a socket address:
;; + IP address
;; + Port number


;; -> Socket Connections
;; Steps:
;; 1. A program to create a socket that starts in a listening state (server)
;; 2. A program to create a socket its end and uses it to establish a connection with the server (client)
;; If all goes well, these two programs can now transmit messages across the socket connection running between them

;; This example will be write on the client and server files.
;; + cap12-socket-server.lisp
;; + cap12-socket-client.lisp


;; :: String Streams: The Oddball Type
(defparameter foo (make-string-output-stream))
(princ "This will go into foo. " foo)
(princ "This will also go into foo. " foo)
(get-output-stream-string foo)


;; :: Reading and Debugging
;; Another reason for using string streams is that they can make our code
;; easier to read and debug, especially when we use the with-output-to-
;; string macro

;; Here's an example

(with-output-to-string (*standard-output*)
  (princ "the sum of ")
  (princ 5)
  (princ " and ")
  (princ 2)
  (princ " is ")
  (princ (+ 2 5)))
;; => the sum of 5 and 2 is 7

;; The with-output-to-string macro will intercept any text that would
;; otherwise be output to the console, REPL, or other output stream,
;; and capture it as a string.

;; As a exercise of this chapter about streams as well about sockets,
;; I've wrote a experimental repository of chat-like system through
;; the local network at www.github.com/ryukinix/lisp-chat



