

(define (extract list xs)
    (if (null? list)
        xs
        (extract (cdr list) (cons (car list) xs))))



(define (my-flatten3 xs)
  (define (loop xs res)
    (if (null? xs)
        (reverse res)
        (if (not (list? (car xs)))
            (loop (cdr xs) (cons (car xs)  res)) 
            (loop (extract  (reverse (car xs)) (cdr xs)) res))))
  (loop xs '()))
(my-flatten3'((1) 2 (3 (4 5)) 6))