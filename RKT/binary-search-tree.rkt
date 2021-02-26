;; r5rs
;; tree:list (length tree) = 3
;; construct method :(list val left right)
;; empty tree = ()
(define (binmap f tree)
(if (null? tree) tree (list (car tree) (f (car (cdr tree))) (f (car (cddr tree))))))

(define (bincataT f) (lambda(t) (f (binmap (bincataT f) t))))

(define (bigger x y) (if (> x y) x y))
;;maybe faultful

(define (preheight x)
  (cond
    ((null? x) 0)
    (else (+ 1 (bigger (car (cdr x)) (car (cddr x)))))))

(define get-bst-height (bincataT preheight))