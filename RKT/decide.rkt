(define (:*: x y) (lambda(f) (f x y)))
(define (fst p) (p (lambda(x y) x)))
(define (snd p) (p (lambda(x y) y)))

(define (mss l)
  (cond
   [(null? l) (:*: 0 0)]
   [else (let ([res (mss (cdr l))])
	   (let ([seed (snd res)]
		 [val (fst res)])
	     (:*: (max (+ seed (car l)) (car l) val) (+ (car l) (max 0 seed)))))])) ;; seed <= val


(define (pos l)
  (foldr
   (lambda(elem acc)
     (append (map (lambda(x) (cons elem x)) acc) acc))
   '(()) l))
