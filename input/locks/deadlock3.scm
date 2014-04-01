(letrec ((a #unlocked)
         (b #unlocked)
         (c #unlocked)
         (t1 (spawn (begin
                      (acquire a)
                      (acquire b)
                      (release b)
                      (release a))))
         (t2 (spawn (begin
                      (acquire b)
                      (acquire c)
                      (release c)
                      (release b))))
         (t3 (spawn (begin
                      (acquire c)
                      (acquire a)
                      (release a)
                      (release c)))))
  (join t1)
  (join t2)
  (join t3))