(letrec ((down (lambda (x)
                 (if (= x 0)
                   "done"
                   (down (- x 1)))))
         (up (lambda (x)
               (if (= x 0)
                 "done"
                 (up (+ x 1)))))
         (x (down 5)))
  x)
