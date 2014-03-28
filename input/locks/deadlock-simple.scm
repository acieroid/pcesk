(letrec ((lock #locked)) ; lock already locked
  (acquire lock)
  1)
