(defmacro def/g(info . body)
  (let ((state (gensym))
	(return (gensym))
	(fn (gensym))
	(cont (gensym)))
    `(define ,info
       (let* ((,state #nil)
	      (,return #nil)
	      (yield
	       (lambda (m)
		 (call/cc
		  (lambda (,cont)
		    (set! ,state ,cont)
		    (,return m)))))
	      (,fn (lambda()
		     ,@body)))
	 (lambda()
	   (call/cc
	    (lambda (,cont)
	      (set! ,return ,cont)
	      (if (eq? ,state #nil)
		  (,fn)
		  (,state #:start)))))))))

(def/g (Range0 n lim)
  (let loop ((acc n))
    (if (< acc lim)
	(begin (yield acc)
	       (loop (+ acc 1))) #f)))	      
