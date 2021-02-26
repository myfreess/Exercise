(define (fact^ f)
  (λ(x)
    (if (= x 0)
	1
	(* x (f (- x 1))))))

(define fact
  ((λ(f) (fact^ (λ(n) ((f f) n))))
   (λ(f) (fact^ (λ(n) ((f f) n))))))
    
