(letrec ((x 1)
         (t1 (spawn (set! x 2)))
         (t2 (spawn (set! x 2))))
  (join t1)
  (join t2))