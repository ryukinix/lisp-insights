;; Common Lisp Script
;; Manoel Vilela

(defun robots ()
  (loop named main
        with directions = '((q . -65) (w . -64) (e . -63) (a . -1)
                            (d .   1) (z .  63) (x .  64) (c . 65))
        for pos = 544
          then (progn (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave: ")
                      (force-output)
                      (let* ((c (read))
                             (d (assoc c directions)))
                        (cond (d (+ pos (cdr d)))
                              ((eq 't c) (random 1024))
                              ((eq 'l c) (return-from main 'bye))
                              (t pos))))
        for monsters = (loop repeat 10
                             collect (random 1024))
          then (loop for mpos in monsters
                     collect (if (> (count mpos monsters) 1)
                                 mpos
                                 (cdar (sort (loop for (k . d) in directions
                                                   for new-mpos = (+ mpos d)
                                                   collect (cons (+ (abs (- (mod new-mpos 64)
                                                                            (mod pos 64)))
                                                                    (abs (- (ash new-mpos -6)
                                                                            (ash pos -6))))
                                                                 new-mpos))
                                             #'<
                                             :key #'car))))
        when (loop for mpos in monsters
                   always (> (count mpos monsters) 1))
          return 'player-wins
        do (format t
                   "~%|~{~<|~%|~,65:;~A~>~}|"
                   (loop for p
                           below 1024
                         collect (cond ((member p monsters)
                                        (cond ((= p pos) (return-from main 'player-loses))
                                              ((> (count p monsters) 1) #\#)
                                              (t #\A)))
                                       ((= p pos) #\@)
                                       (t #\space))))))

(princ (robots))
(fresh-line)
