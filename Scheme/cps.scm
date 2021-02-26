(define reverse/cps
  (lambda(lst k)
	(cond
	  ((null? lst) (k '()))
	  (else (reverse/cps (cdr lst)
						 (lambda(x) (cons (car lst) (k x))))))))

(define map/cps
  (lambda(f lst k)
	(cond
	  ((null? lst) (k '()))
	  (else (map/cps f (cdr lst)
					 (lambda(x)
					   (k (cons (f (car lst)) x))))))))

(define fold/cps
  (lambda(f val lst k)
	(cond
	  ((null? lst) (k val))
	  (else
		(fold/cps f val (cdr lst)
				  (lambda(x)
					(f (k x) (car lst))))))))
