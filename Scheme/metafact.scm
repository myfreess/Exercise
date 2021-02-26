(define tail-fact
  (lambda(origin N)
	(cond ((= origin 0) 1)
		  ((= origin 1) N)
		  (else (tail-fact (- origin 1) (* N origin))))))
(define realfact
  (lambda(origin)
	(cond ((= origin 0) 1)
		  ((= origin 1) 1)
		  (else (* origin (realfact (- origin 1)))))))
