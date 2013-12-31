(letrec ((rng (lambda (seed)
                (letrec ((state seed))
                  (lambda ()
                    (letrec ((old state)
                             ;; Numbers from rand(3) cannot be used
                             ;; since we work with OCaml's 31-bit
                             ;; integers
                             (new (modulo (+ (* 5245 old) 12345) 107374182))
                             (n (modulo (/ new 65535) 32768))
                             (f (lambda ()
                                  (if (cas state old new)
                                      n
                                      (f)))))
                      (f))))))
         (gen (rng 100))
         (t1 (spawn (gen)))
         (t2 (spawn (gen))))
  (gen)
  (join t1)
  (join t2))
