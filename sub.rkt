
(define (f xs)
  (if (list? xs)
      (if (null? xs)
          xs
      (cons  (f (car xs))  (f  (cdr xs)))))
      (if (null? xs)
          xs
          (if (pair? xs)
              (if (pair? (cdr xs))
                  (cons  (f (car xs))  (f  (cdr xs)))
                  (cons  (f (car xs))  (list (f  (cdr xs)))))
              xs)))

(f '(1 (2 (5 6 . 7) . 3) 2 . 3))
(f '(( 1 4 )2 3))
