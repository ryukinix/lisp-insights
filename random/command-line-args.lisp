#!/usr/bin/sbcl
;; Common Lisp Script
;; Manoel Vilela

(defun my-command-line ()
  (or 
   #+SBCL sb-ext:*posix-argv*
   #+CLISP *args*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

(format t "~&~S~&" (my-command-line))