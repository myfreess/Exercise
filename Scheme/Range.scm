(define Range0
  (lambda(n lim)
    (let*
	((state identity)
	 (return (list #:container))
	 (fn
	  (lambda()
	    (let loop ((acc n))
		(if (< acc lim)
		    (begin
		      (call/cc
		       (lambda(cont)
			 (set! state cont)
			 ((car return) acc)))
		      (loop (+ acc 1))))))))
      (lambda()
	(call/cc
	 (lambda(cont)
	   (set-car! return cont)
	  (if (eq? state identity)
	  (fn)
	  (state #nil))))))))


	      
