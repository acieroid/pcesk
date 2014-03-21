(letrec ((lock (= 1 1)) ; lock already locked, but the abstract interpreter won't know it
         (f (lambda ()
              (if (cas lock #f #t)
                nil
                (f)))))
  (f))
