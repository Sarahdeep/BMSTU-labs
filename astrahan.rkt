(define (list-trim-right xs)
  (let
      ((xv (length xs))
       (xk xs))
    (define (loop xs counter)
      (if (null? xs)
          (loop2 xk  (- xv counter))
          (cond
            ((or (not (char? (car xs))) (not (char-whitespace? (car xs))))
             (loop (cdr xs) 0)) 
            (else
             (loop (cdr xs) (+ counter 1))))))
    (loop xs  0)))
(define (loop2 xs  length)
  (if  (equal? length 0)
       '()
       (cons (car xs) (loop2 (cdr xs) (- length 1))  )))
  
