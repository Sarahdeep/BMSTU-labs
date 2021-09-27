(define (my-range a b d)
  (if (< a b)
      (append (list a) (my-range (+ a d) b d ))
      '()))
(display "#1\n" )
(my-range  0 11 3)
(my-range  0 13 3)
(my-range  0 14 3)



              
(define (chill? ibas) (or (null? ibas)
                          (and
                           ( not (list? (car ibas)))
                           (chill? (cdr ibas)))
                          ))

(define (loop xs res)
  (if (not (null? xs))
      (cond 
        ((list? (car xs))
         (loop (cdr xs) (append res (car xs))))
        ((not (list? (car xs)))
         (loop (cdr xs) (append res (list (car xs))))))
      (if (chill? res)
          res (loop res '()))))
(define (my-flatten xs) (loop xs '()))
(display "#2\n" )
(my-flatten '((1) 2 (3 (4 5)) 6))
(my-flatten '((((((((     4)))))))))

(define (my-element? x xs)
  (and (not (null? xs))
       (or (equal? (car xs) x)
           (my-element? x (cdr xs)))))
(display "#3\n" )
(my-element? 1 '(3 2 1 1)) 
(my-element? 4 '(3 2 1))
;
;
;
(define (my-filter f xs)
  (if (null? xs)
      '()
      (if  (f (car xs))
          (cons (car xs) (my-filter f (cdr xs)))
          (my-filter f (cdr xs)))))
(display "#4\n" )
(my-filter odd? (my-range 0 10 1))
(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1))
;
;
;

(define (loop f start finish xs)
                              (if (>= (length xs) 2)
                                  (loop  f (f (car xs) (cadr xs)) (cadr xs) (cdr xs))
                                  start))
(define (my-fold-left f xs)
  (if (> (length xs) 1)
  (loop f (car xs) (cadr xs) xs)
   (car xs)))
(display "#5\n" )
(my-fold-left  quotient '(16 2 2 2 2))
(my-fold-left  quotient '(1))
(my-fold-left expt     '(2 3 4))
;
;
;
(define (my-fold-right f xs)
  (if (> (length xs) 1)
  (loop f (car (reverse xs)) (cadr (reverse xs)) (reverse xs))
   (car xs)))
(display "#6\n" )
(my-fold-right  quotient '(2 2 2 2 16))
(my-fold-right expt     '(2 3 4))  
(my-fold-right expt     '(2)) 