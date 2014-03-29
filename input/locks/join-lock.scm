(letrec ((t1 (spawn (join t2)))
         (t2 (spawn (join t1))))
  (join t1)
  (join t2))
