(letrec ((counter 0)
         (f (lambda ()
              (letrec ((old counter)
                       (new (+ counter 1)))
                (if (cas counter old new)
                  "done"
                  (f)))))
         (t1 (spawn (f))))
  (join t1)
  counter)

