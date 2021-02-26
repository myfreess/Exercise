(define (concatMap f l)
  (foldr
   (lambda(x acc) (append (f x) acc))
   '() l))

(define (: x) (lambda(xs) (cons x xs)))

(define (insert elem)
  (lambda(l)
  (let loop
      ([acc '()]
       [prev identity]
       [l l])
    (cond
     [(null? l) (cons (prev (cons elem l)) acc)]
     [else
      (loop (cons (prev (cons elem l)) acc) (compose prev (: (car l))) (cdr l))]))))
	     
       

(define (perm l)
  (foldr (lambda(x acc)
	   (concatMap (insert x) acc)) '(()) l))


;; CPS Version

(define (concatMapk f l)
    (foldr
     (lambda(x acc) (compose (f x) acc))
     identity l))

(define (insertK elem)
  (lambda(l)
  (let loop
      ([acc identity]
       [prev identity]
       [l l])
    (cond
     [(null? l) (compose (: (prev (cons elem l))) acc)]
     [else
      (loop (compose (: (prev (cons elem l))) acc) (compose prev (: (car l))) (cdr l))]))))

(define (perm^ l)
  (foldr
   (lambda(x acc) ((concatMapk (insertK x) acc) '())) '(()) l))
