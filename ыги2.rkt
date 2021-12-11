(define (and-fold . xs )
  (or
   (null?  xs)
   (and (car (revxs) (apply and-fold (cdr xs)))))

(apply and-fold '(#t #t 5))




(define (and-fold2 . xs) (define a #t)
  (define (loop x res)
    (if (null? x)
        res
        (loop  (cdr x) (and res (car x)))))

  (loop (cons a xs) a))
    
