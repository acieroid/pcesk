;; Taken from https://github.com/jensnicolay/abstractmemo
;; Expected result: #f
((lambda (f1)
   (letrec ((a (f1 #t)))
     (f1 #f)))
 (lambda (x1)
   ((lambda (f2)
      (letrec ((b (f2 #t))
               (c (f2 #f)))
        (f2 #t)))
    (lambda (x2) ((lambda (z) (z x1 x2)) (lambda (y1 y2) y1))))))
