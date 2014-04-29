(letrec ((lock #unlocked)
         (t1 (spawn (acquire lock))))
  (acquire lock)
  (join t1))
