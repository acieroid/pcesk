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
                      ; forgot to release the lock!
                      ))))
  (acquire)
  (join t1))
