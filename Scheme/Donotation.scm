(define-syntax do/m
  (syntax-rules (using <-)
    [(_) #nil]
    [(_ [using bind] m) m]
    [(_ [using bind] (var <- m) more ...)
     (bind m (lambda (var)
                (do/m [using bind] more ...)))]
    [(_ [using bind] m more ...)
     (bind m (lambda (_)
                (do/m [using bind] more ...)))]))
