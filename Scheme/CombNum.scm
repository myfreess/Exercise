(define Comb ((lambda()

(define tail-fact
  (lambda(origin N)
	(cond ((= origin 0) 1)
		  ((= origin 1) N)
		  (else (tail-fact (- origin 1) (* N origin))))))

(define f!
  (lambda(origin)
	(tail-fact origin 1)))


(define C!
  (lambda(N M)
	(/ (f! N) (* (f! M) (f! (- N M))))))

(define wrapper
  (lambda(N M)
	(cond ((and (> N 0) (> N M)) (C! N M)) (else '()))))

wrapper

)))







	
