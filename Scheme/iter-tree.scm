(eval-when (compile)
		   (load "Generator.scm"))
;;只适合guile
(def/g (tree/iter tree)
	   (let loop ((tree tree))
		 (cond
		   ((null? tree) '())
		   ((pair? tree)
			(loop (car tree))
		    (loop (cdr tree)))
		   (else (yield tree)))))
;;我不明白……
