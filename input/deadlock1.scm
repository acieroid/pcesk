;; Deadlock involving only one lock, but two threads
(letrec ((lock #f)
         (acquire (lambda ()
                    (if (cas lock #f #t)
                        nil
                        (acquire))))
         (release (lambda ()
                      (set! lock #f)))
         (t1 (spawn (begin
                      (acquire)
                      1 ; forgot to release the lock!
                      ))))
  (acquire)
  2
  (release)
  (join t1))
