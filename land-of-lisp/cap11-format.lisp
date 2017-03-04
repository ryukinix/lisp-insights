;; Common Lisp Script
;; Manoel Vilela

;; :: Anatomy of the format function
(format t "Add onion rings for only ~$ dollars more!" 1.5)
;; ARCH => (format <the-destination-parameter> <the-control-string> &rest <parameters>)
;; STDOUT => "Add onion rings for only 1.50 dollars more"
;; RETURNS => nil

;; the destination parameter
;;    nil => don't print anything; just return the value as a string
;;      t => print the value to the console. In this case, the function just returns nil as a value (as in the example above)
;; stream => write the data to an output stream

(princ (reverse
        (format nil "Add onion rings for only ~$ dollars more" 1.5)))
;; RETURNS => "erom srallod 05.1 ylno rof sgnir noino ddA"

;; :: The control string parameter

;; ~$ is a control sequence which indicates a `monetary floating-point` value
;; NOTE: Every control sequence recognized by the format functions begins with the tilde (~) character.

;; control sequences for printing lisp values

(prin1 "foo") ;; => "FOO"
(princ "foo") ;; => FOO

;; prin1 => machine usage
;; princ => human readable

;; A alternative way can be wrote using the format function with ~s and ~a
;; control sequences.

(format t "I am printing ~s in the middle of this sentence." "foo")
;; => I am printing "foo" in the middle of this sentence.
(format t "I am printing ~a in the middle of this sentence." "foo")
;; => I am printing foo in the middle of this sentence.


;; For example, by writing ~10a in the following example, we add seven spaces
;; to the right of foo, making the total width of the formatted value 10 chars.

(format t "I am printing ~10a within ten spaces of room." "foo")
;; => I am printing foo        within ten spaces of room.

;; We can also add spaces on the left side of the value by adding the @ symbol.

(format t "I am printing ~10@a within ten spaces of room." "foo")
;; => I am printing        foo within ten spaces of room.

;; control sequences can accept more than just one parameter.
;; Let's look at an example that sets the second param of the ~a control sequence as well:

(format t "I am printing ~10,3a within then (or more) spaces of room." "foo")
;; => I am printing foo          within then (or more) spaces of room.

;; As you can see, additional parameters to a control sequence are separated
;; with a comma. In this case, the second parameter is set to 3. Which tells
;; the format command to add spaces in groups of three until the goal width
;; of 10 is reached. In this example, a total of nine spaces are added to the
;; formatted value. This means it overshot our goal width of 10, leading
;; instead to a total width of 12. But using this feature is rare.

;; As well, a control sequence can has a third argument.

(format t "I am printing ~,,4a in the middle of this sentence." "foo")
;; => I am printing foo     in the middle of this sentence.

;; Exactly four spaces is added together with the foo symbol.

;; The fourth control sequence parameter specifies which character will be used
;; for padding. For example, in the following listing, we pad the printed
;; value with four exclamation points:

(format t "The word ~,,4,'!a feels very important." "foo")
;; => The word foo!!!! feels very important.

;; We can combine the @ symbol with this example too.

(format t "The word ~,,4,'!@a feels very important." "foo")
;; => The word !!!!foo feels very important.


;; :: Control Sequences for Formatting Numbers

;; the format command has many options designed specifically for controlling
;; the appearance of numbers. Let's look at some of the more useful ones.

;; -> Control Sequence for Formatting Integers
;; First, we can use format to display a number using a different base. For
;; instance we can display a number in hexadecimal (base-16) with the ~x control
;; sequence

(format t "The number 1000 in hexadecimal is ~x" 1000)
;; => The number 1000 in hexadecimal is 3E8

;; Similarly, we can display a number in binary (base-2) using the ~b control
;; sequence

(format t "The number 1000 in binary is ~b" 1000)
;; => The number 1000 in binary is 1111101000

;; We can even explicitly declare that a value will be displayed as a decimal
;; (base-10) number, using the ~d control sequence:
(format t "The number 1000 in decimal is ~d" 1000)
;; => The number 1000 in decimal is 1000

;; The difference is that ~d supports special parameters and flags
;; that are specific to printing decimal numbers. For example, we can
;; place a colon inside the control sequence to enable commas as
;; digit group separators.

(format t "Numbers with commas in them are ~:d times better." 1000000)
;; => Numbers with commas in them are 1,000,000 times better.

;; To control the width of the number, we can set the padding parameter, just
;; as we did with the ~a and ~s control sequences

(format t "I am printing ~10d within ten spaces of room" 1000000)
;; => I am printing    1000000 within ten spaces of room

;; To change the character used for padding, pass in the desired character
;; (in this case, the x character) as the second parameter:

(format t "I am printing ~10,'xd within ten spaces of room" 1000000)
;; => I am printing xxx1000000 within ten spaces of room


;; :: Control Sequences for Formatting Floating-Point Numbers

;; Floating-point values are handled with the ~f control sequence.
;; Controlling precision by max width size can be done:

(format t "PI can be estimated as ~4f" 3.141593)
;; => PI can be estimated as 3.14


;; The second parameter of the ~f control sequence controls the number
;; of digits displayed after the decimal point. For example, if we pass 4 as
;; the second parameter in the preceding example, we get the following output:

(format t "PI can be estimated as ~,4f" 3.141593)
;; => PI can be estimated as 3.1416

;; The third parameter of the ~f control sequence causes the number to
;; to be scaled by factors of ten. For example, we can pass 2 as the third parameter,
;; which we can use to multiply a fraction by 10² to turn it into a percentage
(format t "Percentages are ~,,2f% better than fractions" 0.77)
;; => Percentages are 77.0% better than fractions

;; In addition to ~f, we can use the control sequence ~$, which is used for
;; formatting currencies
(format t "I wish I had ~$ dollars in my bank account." 1000000.2)
;; => I wish I had 1000000.20 dollars in my bank account.

;; :: Printing Multiple Lines of Output

;; Common Lisp has two different commands for starting a new line during priting.
;; The first: terpri
;; Simply tells LISP to terminate the current line and start a new one for printing
;; subsequent output. For example, we can print two numbers on different lines like so.

(progn (princ 22)
       (terpri)
       (princ 33))

;; STDOUT => 22
;;           33

;; We can also start a new line with fresh-line. This command will start a new line,
;; but only if the cursor position in the REPL isn't already at the very front of a line.
;; This is seems be tricky, so let's look at some examples:

(progn (princ 22)
       (fresh-line)
       (princ 33))

(progn (princ 22)
       (fresh-line)
       (fresh-line)
       (princ 33))

;; The above programs has the same output:
;; STDOUT => 22
;;           33
;; In another words, terpri always print a new line; fresh-line only prints when is needed.


;; The format command has two control sequences equivalents to terpri and fresh-line
;; NOTE: WHAT THE FUCK!? WHY TERPRI AS NAME OF THIS TYPE OF FUNCTION? IS SO NON-SENSE.

;; ~% causes a new line to be created in all cases  (like terpri)
;; ~& creates new lines only as needed (like fresh-line)

;; These examples illustrate this difference:

(progn (format t "this is on one line ~%")
       (format t "~%this is on another line"))
;; STDOUT =>
;; this is on one line
;;
;; this is on another line

(progn (format t "this is on one line ~&")
       (format t "~&this is on another line"))
;; STDOUT =>
;; this is on one line
;; this is on another line

;; As you can see, using an extra ~% prints an unsightly empty line and
;; using ~& in the same places does not.

;; These two line-termination sequences can also have an additional parameter
;; in front of them to indicate the number of new lines to be created. This is
;; useful in cases where we want to use empty lines to space out our output.
;; For example, the addition of 5 in the following examples adds five empty lines
;; to our output:

(format t "this will print ~5% two lines spread far apart")
;; STDOUT =>
;; this will print
;;
;;
;;
;;
;; two lines spread far apart


;; :: Justifying Output

(defun random-animal ()
  (nth (random 5)
       '("dog" "tick" "tiger" "walrus" "kangaroo")))

;; now suppose we want to display a bunch of random animals in a table.
;; We can do this by using the ~t control sequence. ~t can take a parameter
;; that specifies the column position at which the formatted value should appear.
;; For example, to have our table of animals appear in three columns at the fifth,
;; fifteenth and twenty-fifth character positions, we could create this table

(loop repeat 10
      do (format t "~5t~a ~15t~a ~25t~a~%"
                 (random-animal)
                 (random-animal)
                 (random-animal)))

;; STDOUT =>
;; dog       tick      walrus
;; kangaroo  tiger     walrus
;; dog       dog       dog
;; tick      tick      tiger
;; walrus    kangaroo  dog
;; walrus    dog       tick
;; dog       kangaroo  dog
;; dog       walrus    tiger
;; kangaroo  walrus    walrus
;; walrus    tick      tick


;; Now suppose we want all the animals be spaced equally apart on a single line.
;; To do so, we can use the ~< and ~> control sequences, as follows:

(loop repeat 10
      do (format t "~30<~a~;~a~;~a~>~%"
                 (random-animal)
                 (random-animal)
                 (random-animal)))
;; STDOUT =>
;; tick      tiger       kangaroo
;; kangaroo     walrus      tiger
;; tiger      kangaroo      tiger
;; tiger      kangaroo      tiger
;; walrus       tiger        tick
;; kangaroo      tick       tiger
;; tiger       walrus       tiger
;; dog        walrus         tick
;; tiger         tick         dog
;; dog        dog        kangaroo


;; VERY VERY TRICKY
;; ~< and ~> is used to start and finish a text justifying operation

;; For example, we can create a single, neatly centered column as follows:

(loop repeat 10
      do (format t "~30:@<~a~>~%" (random-animal)))


;; In the same way we can use :@ with multiple justified values, centering
;; them on the line with additional space at their left and right ends.

(loop repeat 10
      do (format t "~30:@<~a~;~a~;~a~>~%"
                 (random-animal)
                 (random-animal)
                 (random-animal)))

;; To produce neat collums, we'll still use the :@ flag, but we'll describe
;; our rows using three separate 10-character justification sections

(loop repeat 10
      do (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%"
                 (random-animal)
                 (random-animal)
                 (random-animal)))

;; STDOUT =>
;;   kangaroo   walrus     dog
;;   kangaroo   walrus   kangaroo
;;    tiger      dog       dog
;;   kangaroo   tiger      dog
;;     dog       tick     tiger
;;    walrus    tiger     tiger
;;    walrus     dog       dog
;;   kangaroo   walrus     dog
;;    walrus     tick      tick
;;    tiger      tick     walrus


;; :: Iterating Through Lists Using Control Sequences

;; Format can loop through data using the ~{ and ~} control sequences

(defparameter *animals* (loop repeat 10 collect (random-animal)))

(format t "~{I see a ~a! ~}" *animals*)
;; STDOUT => I see a tick! I see a dog! I see a dog! I see a kangaroo! I see a tick! I see a dog! I see a walrus! I see a tick! I see a dog! I see a walrus!

;; To produce this loop, we simply pass the single variable *animals*, a list of items,
;; to the format function. The control string iterates through the list, constructing
;; the sentence "I see a ~a" for each member of *animals*.

;; A single iteration construct can also grab more than one item from the list,
;; as in this example:

(format t "~{I see a ~a... or was it a ~a?~%~}" *animals*)
;; STDOUT =>
#|
I see a tick... or was it a dog?
I see a dog... or was it a kangaroo?
I see a tick... or was it a dog?
I see a walrus... or was it a tick?
I see a dog... or was it a walrus?
|#

;; We need be careful in this example above. If the number of elements is odd this
;; will cause a failure because the data is accessed by pairs.

;; :: A Crazy Formatting Trick for Creating Pretty Tables of Data

(format t "|~{~<|~%|~,33:;~2d ~>~}|" (loop for x below 100 collect x))
;; STDOUT =>
;; | 0  1  2  3  4  5  6  7  8  9 |
;; |10 11 12 13 14 15 16 17 18 19 |
;; |20 21 22 23 24 25 26 27 28 29 |
;; |30 31 32 33 34 35 36 37 38 39 |
;; |40 41 42 43 44 45 46 47 48 49 |
;; |50 51 52 53 54 55 56 57 58 59 |
;; |60 61 62 63 64 65 66 67 68 69 |
;; |70 71 72 73 74 75 76 77 78 79 |
;; |80 81 82 83 84 85 86 87 88 89 |
;; |90 91 92 93 94 95 96 97 98 99 |

;; NEAT!
