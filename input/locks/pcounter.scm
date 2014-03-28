(letrec ((lock #unlocked)
         (counter 0)
         (inc (lambda ()
                (acquire lock)
                (set! counter (+ counter 1))
                (release lock)))
         (t1 (spawn (inc)))
         (t2 (spawn (inc))))
  (join t1)
  (join t2)
  counter)
