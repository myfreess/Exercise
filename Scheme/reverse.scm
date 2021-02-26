(define reverse
  (lambda(result lst)
    (cond
     ((null? lst) result)
     (else (reverse (cons (car lst) result) (cdr lst))))))

(define reverse-cps
;;(reverse-cps <lat> identity))
  (lambda(lst k)
    (cond
     ((null? lst) (k '()))
     (else (reverse-cps (cdr lst) (lambda(x) (cons (car lst) (k x))))))))
