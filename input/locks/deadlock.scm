(letrec ((a #unlocked)
         (b #unlocked)
         (t1 (spawn (begin
                      (acquire a)
                      (acquire b)
                      (release b)
                      (release a))))
         (t2 (spawn (begin
                      (acquire b)
                      (acquire a)
                      (release a)
                      (release b)))))
  (join t1)
  (join t2))
