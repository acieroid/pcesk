(letrec ((lock #unlocked)
         (t1 (spawn (acquire lock))))
  (acquire lock)
  (release lock)
  (join t1))
