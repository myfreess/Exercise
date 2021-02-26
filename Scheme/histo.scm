;; Attr f a = (a, f[Attr f a])

(use-modules (srfi srfi-9))

(define-record-type <Attr>
  (make-attr value memo)
  attr?
  (value elim-value)
  (memo elim-memo))

(define $mapRec)

(define (histoT h)
  (letrec ([w (λ(x)
		(let ([val ($mapRec w x)])
		  (make-attr (h val) val)))])
    (λ(e) (elim-value (w e)))))

;; Nat a = 0 | (cons 1 a)


;;pred: a -> bool
(define (one? x) (and (integer? x) (= x 1)))
(define (zero? x) (and (integer? x) (= x 0)))

(define (non-z-nat? x) (and (pair? x) (one? (car x))))

(define (nat-map f n)
  (cond
   [(zero? n) n]
   [(non-z-nat? n)
    (cons 1 (f (cdr n)))]
   [else (error "Not a Natural Number")]))

(define (naT n)
  (cond
   [(zero? n) n]
   [else (cons 1 (naT (- n 1)))]))

(define (fib n)
  (begin
    (set! $mapRec nat-map)
  ((histoT
   (λ(m)
     (cond
      [(zero? m) m]
      [(zero? (elim-memo (cdr m))) 1]
      [else
       (let* ([r (cdr m)]
	      [n1 (elim-value r)]
	      [n2 (elim-value
		   (cdr (elim-memo r)))])
	 (+ n1 n2))]))) (naT n))))
	     
    
