(letrec ((counter 0)
         (f (lambda ()
              (letrec ((old counter)
                       ;; Bug introduced here
                       (new (+ counter 1)))
                (if (cas counter old new)
                  "done"
                  (f)))))
         (t1 (spawn (f)))
         (t2 (spawn (f))))
  (join t1)
  (join t2)
  counter)
