;; Taken from https://github.com/jensnicolay/abstractmemo
;; Expected result: #t
(letrec ((id (lambda (x) x))
         (blur (lambda (y) y))
         (lp (lambda (a n)
               (if (<= n 1)
                 (id a)
                 (letrec ((r ((blur id) #t))
                          (s ((blur id) #f)))
                   (not ((blur lp) s (- n 1))))))))
      (lp #f 2))
