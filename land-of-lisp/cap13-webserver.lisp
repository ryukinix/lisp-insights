;; Common Lisp Script
;; Manoel Vilela

;; :: Writing a Web Server from Scratch

;; -> How a Web Server Works

;; Hypertext Transfer Protocol, or HTTP, is the internet protocol
;; used for transferring web pages. It adds a layer on top of TCP/IP
;; for requesting pages once a socket connection has been established.
;; When a program running on a client computer (usually a web browser)
;; sends a properly encoded request, the server will retrieve the requested
;; page and send it over the socket stream in response.

;; NOTE: this web server is adapted from 'http.lisp' created by Ron Garret.

;; Decoding the Values of Request Parameters

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
               (coerce (list c1 c2) 'string)
               :radix 16
               :junk-allowed t)))
    (if code
        (code-char code)
        default)))

(defun decode-param (s)
  (labels ((f (list)
             (when list
               (case (car list)
                 (#\% (cons (http-char (cadr list) (caddr list))
                            (f (cdddr list))))
                 (#\+ (cons #\space (f (cdr list))))
                 (otherwise (cons (car list) (f (cdr list))))))))
    (coerce (f (coerce s 'list))
            'string)))

;; Unit tests
(decode-param "foo")
;; => "foo"
(decode-param "foo%3F")
;; => "foo?"
(decode-param "foo+bar")
;; => "foo bar"

;; :: Decoding lists of request parameters

(defun parse-params (s)
  (let ((i1 (position #\= s))
        (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

(parse-params "name=bob&age=25&gender=male")
;; => ((NAME . "bob") (AGE . "25") (GENDER . "male"))

;; NOTE: Both decode-param and parse-params could achieve higher performance
;; if they were written using a tail call, as we'll discuss in Chapter 14

(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\space s))
                      (position #\space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url '()))))

(parse-url "GET /lolcats.html HTTP/1.1")
;; => ("lolcats.html")
(parse-url "GET /lolcats.html?extra-funny=yes HTTP/1.1")
;; => ("lolcats.html" (EXTRA-FUNNY . "yes"))


(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
              (when i
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

;; :: Testing get-header with a String stream

(get-header (make-string-input-stream "foo: 1
bar: abc, 123

"))
;; => ((FOO . "1") (BAR . "abc, 123"))
;; In that example we simulated a socket stream using a string stream with
;; input direction. Nice!

;; => Parse the Request Body

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))

(ql:quickload 'usocket)
(use-package 'usocket)

;; The server function is briefly modified to working with SBCL and USOCKET

(defun serve (request-handler)
  (let ((socket (socket-listen "localhost" 8080)))
    (unwind-protect
         (loop (with-open-stream (stream (socket-stream (socket-accept socket)))
                 (let* ((url    (parse-url (read-line stream)))
                        (path   (car url))
                        (header (get-header stream))
                        (params (append (cdr url)
                                        (get-content-params stream header)))
                        (*standard-output* stream))
                   (funcall request-handler path header params))))
      (socket-close socket))))


;; :: Building a Dynamic Website

;; To try out our shiny new web server, let's build a simple site that
;; greets a visitor, using the dirt-simple function hello-request-handler

(defun hello-request-handler (path header params)
  (declare (ignore header))
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
        (if (not name)
            (princ "<html><form>What is your name?<input name='name'/>
                    </form></html>")
            (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
      (princ "ERROR 404 - Sorry... I don't know that page")))

(hello-request-handler "lolcats" '() '())
;; => ERROR 404 - Sorry... I don't know that page

(hello-request-handler "greeting" '() ())
;; => <html><form>What is your name?<input name='name'/></form></html>

;; Executing the server...
(serve #'hello-request-handler)
;; You can access this server on
;; 127.0.0.1:8080/greeting

;; We now have a fully functioning web server and request handling infrastructure.
