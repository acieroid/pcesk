(letrec ((counter 0)
         (f (lambda ()
              (letrec ((old counter)
                       (new (+ counter 1)))
                (if (cas counter old new)
                  #t
                  (f)))))
         (t1 (spawn (f))))
  (join t1)
  (letrec ((t2 (spawn (f))))
    (join t2))
  counter)
