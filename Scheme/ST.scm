;;State Monad by scheme
;;Author:NeoKF
;;Date:


(use-modules (srfi srfi-9))
(read-enable 'curly-infix)


(define-record-type <product>
  (:*: e1 e2)
  product?
  (e1 fst)
  (e2 snd))

(define-record-type <state>
  (make-state s)
  state?
  (s elim-state))

(define void)

(define-syntax ST
  (syntax-rules (-> :)
    [(_ [s : pred? -> body : pred^?])
     (make-state
      (λ(s)
	(if (pred? s)
	    (begin
	      (let ([r31415926 body])
		(if (product? r31415926)
		    (if (pred^? (snd r31415926))
			r31415926
			(error "\n返回值中的新状态类型与原状态不同:" (snd r31415926)))
		    (error "\n返回值类型并非product:" r31415926))))
	    (error "\n接收的状态State^init与期望类型不符:" s))))]))

;;T:State s x
;;s:s
(define (runST T s)
  ((elim-state T) s))

(define (state/bind m f)
  (ST [s : (const #t) ->
    (let* ([x (runST m s)]
	   [v (fst x)]
	   [s^ (snd x)])
      (runST [f v] s^)) : (const #t)]))

(define (state/pure x)
  (ST [s : (const #t) -> (:*: x s) : (const #t)]))

(define getS
  (ST [s : (const #t) -> (:*: s s) : (const #t)]))

(define (putS s^)
  (ST [s : (const #t) -> (:*: void s^) : (const #t)]))


  
