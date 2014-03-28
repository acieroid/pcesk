(letrec ((lock #unlocked)
         (counter 0)
         (inc (lambda ()
                (acquire lock)
                (set! counter (+ counter 1))
                (release lock)))
         (t1 (spawn (inc))))
  (inc)
  (join t1)
  counter)
