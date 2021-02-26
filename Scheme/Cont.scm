(eval-when (compile) (load "Donotation.scm"))
(define-syntax ^$
  (syntax-rules ()
    [(_ kont expr)
     (lambda (kont) (kont expr))]))

(define (liftC x)
  (^$ k x))

(define (bindC receiver f)
  (lambda(k)
    (receiver (lambda(x) ((f x) k)))))

(define (square x) (* x x))

;;(+ 6 (square 6))
(define endness
  (do/m [using bindC]
      [x <- (^$ k (square 6))]
      (^$ k (+ 6 x))))
(define (product l)
  (cond
   [(null? l) (^$ k 1)]
   [else (do/m [using bindC]
	       [x <- (product (cdr l))]
	       (^$ k (* (car l) x)))]))
;scheme@(guile-user)> (product '(1 2 3))
;$3 = #<procedure 76488e9e00 at /data/data/com.termux/files/home/RCFF/Base/Cont.scm:11:2 (k)>
;scheme@(guile-user)> ($3 identity)
; $4 = 6	
	


