(letrec ((lock #t) ; lock already locked
         (f (lambda ()
              (if (cas lock #f #t)
                nil
                (f)))))
  (f))
  
