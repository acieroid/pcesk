(letrec ((lock-a #f)
         (lock-b #f)
         (acquire-a (lambda ()
                      (if (cas lock-a #f #t)
                          nil
                          (acquire-a))))
         (release-a (lambda ()
                      (set! lock-a #f)))
         (acquire-b (lambda ()
                      (if (cas lock-b #f #t)
                          nil
                          (acquire-b))))
         (release-b (lambda ()
                      (set! lock-b #f)))
         (t1 (spawn (begin
                      (acquire-a)
                      (acquire-b)
                      (release-b)
                      (release-a)))))
  (acquire-b)
  (acquire-a)
  (release-a)
  (release-b)
  (join t1))
