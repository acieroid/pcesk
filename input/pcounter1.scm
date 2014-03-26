(letrec ((counter 0)
         (f (lambda ()
              (letrec ((old counter)
                       (new (+ old 1)))
                (if (cas counter old new)
                  #t
                  (f)))))
         (t1 (spawn (f))))
  (join t1)
  counter)

