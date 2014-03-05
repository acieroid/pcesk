(letrec ((x 0)
         (t1 (spawn (cas x 0 1)))
         (t2 (spawn (cas x 0 2))))
  (join t1)
  (join t2)
  x)
  
