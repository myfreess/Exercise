(define (foldr f lst e)
  (cond
   [(null? lst) e]
   [else (f (car lst) (foldr f (cdr lst) e))]))

(define (foldl f acc lst)
  ([foldr
   (λ(e k)
     (compose k (λ(x) (f x e)))) lst identity] acc))

