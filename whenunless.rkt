(define-syntax when
  (syntax-rules ()
    ((when cond? expr1  expr2 ...)
     (if cond?
         (begin 
           expr1 expr2 ...)))
    ;((when cond? expr1)
    ;(if cond?
    ;   expr ))))
    ))
(define x 1)
(when   (> x 0) (display "x > 0") (newline))
(define-syntax unless
  (syntax-rules ()
    ((when  cond? expr1  expr2 ...)
     (and (not cond?)
          (begin 
            expr1 expr2 ...)))))
(unless (= x 0) (display "x != 0") (newline))