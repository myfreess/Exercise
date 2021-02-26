;; (data PreList (a b)
;;       Nil
;;       =========
;;       Cons a b )
;; (define IntList (PreList Int))
(define ($mapRec f lst)
  (cond
   [(null? lst) lst]
   [else (cons (car lst) (f (cdr lst)))]))
(define (FoldG f)
  (lambda(lst)
    (f ($mapRec (FoldG f) lst))))
(define (wrapperT f e)
  (lambda(prelst)
    (cond
     [(null? prelst) e]
     [else (f prelst)])))
;;mult/pre : IntList Int -> Int
(define (mult/pre cell)
  (* (car cell) (cdr cell)))
(define product
  (FoldG (wrapperT mult/pre 1)))
(define (foldr* f lst e)
  ((FoldG
   (wrapperT
    (lambda(cell)
      (f (car cell) (cdr cell))) e)) lst))
;;确实简单
(define (foldr f lst e)
  (cond
   [(null? lst) e]
   [else (f (car lst) (foldr f (cdr lst) e))]))

(define (foldl f e lst)
  (cond
   [(null? lst) e]
   [else (foldl f (f e (car lst)) (cdr lst))]))

;;f:a -> (b . a)||()
(define (unfoldr f e)
  (let ([r (f e)])
    (cond
     [(null? r) r]
     [else (cons (car r) (unfoldr f (cdr r)))])))
;;ugly,(range0/r 3)->(2 1 0)
(define (range0/r n)
  (unfoldr
   (lambda(num)
     (if (= num -1)
	 '()
	 (cons num (- num 1))))
   (- n 1)))


