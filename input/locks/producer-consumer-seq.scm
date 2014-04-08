(letrec ((buffer nil)
         (done #f)
         (lock #unlocked)
         (do-something (lambda (element)
                         ;; Function called on the consumed elements
                         nil))
         (producer (lambda (n)
                     ;; Produce numbers from n to 0, storing them in buffer
                     (if (= n 0)
                         (set! done #t) ; will only work with one producer
                         (begin
                           (acquire lock)
                           (set! buffer (cons n buffer))
                           (release lock)
                           (producer (- n 1))))))
         (consumer (lambda ()
                     ;; Consume an element of buffer until done is #t
                     (if done
                         nil
                         (if (empty? buffer)
                             (consumer)
                             (begin
                               (acquire lock)
                               (do-something (car buffer))
                               (set! buffer (cdr buffer))
                               (release lock)
                               (consumer))))))
         (producer-thread (spawn (producer 5)))
         (_ (join producer-thread))
         (consumer-thread (spawn (consumer))))
  (join consumer-thread))
