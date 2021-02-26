(define resume)

(define-syntax try
  (syntax-rules (catch)
    [(_ thunk catch handler)
     (call/cc (λ(k1)
		(call/cc (λ(k2)
			   (set! resume (λ(msg)
					  (handler msg)
					  (k2 (if #f #f))))
			   (k1 (thunk))))))

			 ]))


(define (weak-err msg)
  (resume msg))

(define (div x y)
  (if (zero? y)
      (weak-err "divide by zero, stop")
      (/ x y)))

(define (test)
  (try (λ() (+ 5 (div 5 0)))
       catch
       (λ(msg) (display "ERROR: ") (display msg) (newline))))
		
  
