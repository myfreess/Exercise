(define (lazy-fix F:thunk)
  ((force F:thunk)
   (delay (lazy-fix F:thunk))))
(define metafact
  (lambda(F:thunk)
    (lambda(n)
      (cond
       [(= n 0) 1]
       [(= n 1) 1]
       [else (* n
		((force F:thunk)
		 (- n 1)))]))))
