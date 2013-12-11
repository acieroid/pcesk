;; Producer-consumer example with one concurrent producer and consumer
(letrec ((buffer nil)
         (done #f)
         (do-something (lambda (element)
                         ;; Function called on the consumed elements
                         nil))
         (producer (lambda (n)
                     ;; Produce numbers from n to 0, storing them in buffer
                     (if (= n 0)
                         (set! done #t) ; will only work with one producer
                         (letrec ((old buffer)
                                  (new (cons n old)))
                           (producer (if (cas buffer old new) (- n 1) n))))))
         (consumer (lambda ()
                     ;; Consume an element of buffer until done is #t
                     (if done
                         nil
                         (if (empty? buffer)
                             (consumer)
                             (letrec ((old buffer)
                                      (element (car old))
                                      (new (cdr old)))
                               (cas buffer old new)
                               (consumer))))))
         (producer-thread (spawn (producer 5)))
         (consumer-thread (spawn (consumer))))
  (join producer-thread)
  (join consumer-thread))
