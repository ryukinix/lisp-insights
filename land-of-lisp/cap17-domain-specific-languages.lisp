;; Common Lisp Script
;; Manoel Vilela

;; On this chapter we'll create a SVG handler DSL using macros
;; Remember SVG (Scalable Vector Graphics) are XML-like,


(defmacro split (val yes no)
  "Exported from the chapter 16"
  (let ((g (gensym)))
        `(let ((,g ,val))
               (if ,g
                   (let ((head (car ,g))
                         (tail (cdr ,g)))
                     ,yes)
                   ,no))))


(defun pairs (lst)
  "Exported from the chapter 16"
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail)
                           (cons (cons head (car tail))
                                 acc))
                        (reverse acc))
                    (reverse acc))))
    (f lst nil)))

;; so we need create tags
(defun print-tag (name alist closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\""
                  (string-downcase (car att))
                  (cdr att)))
        alist)
  (princ #\>))


(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x)
                                       `(cons ',(car x)
                                              ,(cdr x)))
                                     (pairs atts)))
                     nil)
          ,@body
          (print-tag ',name nil t)))

;; using syntax-sugar for html

(defmacro html (&body body)
  `(tag html ()
     ,@body))

(defmacro body (&body body)
  `(tag body ()
        ,@body))
