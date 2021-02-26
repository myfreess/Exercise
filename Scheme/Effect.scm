;;Algebraic Effect by scheme (guile)
;;author : myfreess@github
;;date : 2020 7 30 
(use-modules (ice-9 control))
(use-modules (srfi srfi-9))

(define-record-type <effect>
  (make-effect val k)
  eff*k?
  (val elim-val)
  (k elim-k))
;;低仿Either
(define Fail)

(define (success? x)
  (not (eq? Fail x)))

(define (map-succ f l)
  (cond
   [(null? l) l]
   [else
    (let ([result (f (car l))])
      (if (success? result)
	  (cons result
		(map-succ f (cdr l)))
	  (map-succ f (cdr l))))]))

(define (Yield x)
  (shift k (make-effect x k)))

(define-syntax G^
  (syntax-rules ()
    [(_ body ...)
     (reset body ...)]))

(define (Range n lim)
  (G^ (let loop ((acc n))
	(if (< acc lim)
	    (begin (Yield acc)
		   (loop (+ acc 1))) #f))))



(define perform Yield)


(define (applyE op eff)
  (op (elim-val eff) (elim-k eff)))

(define (Algebra! g handler)
  (if (eff*k? g)
      (Algebra! (applyE handler g) handler)
      g))

(define-syntax runExpr
  (syntax-rules (<==>)
    [(_ Expr <==> handler)
     (Algebra! (G^ Expr) handler)]))


(define (ambC x k)
;;收集所有可能的解
  "x:List"
  (map-succ
   (λ(e)
     (let ([y (k e)])
;;y : Eff | common value (not void)    
       (cond
	[(eff*k? y) (applyE ambC y)]
	[else y]))) x))	
	   

(define-syntax Non-Det
  (syntax-rules ()
    [(_ May)
     (runExpr May
	      <==> ambC)]))
	      
	      




