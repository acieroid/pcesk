(letrec ((x 0)
         (t1 (spawn (set! x 2)))
         (t2 (spawn (cas x 2 1))))
  (join t1)
  (join t2)
  x)
  
