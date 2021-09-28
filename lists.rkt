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
(define (my-flatten2 xs) (loop xs '()))
(display "#2\n"  )
(my-flatten2 '((1) 2 (3 (4 5)) 6))
(my-flatten2 '((((((((     4)))))))))



(define (my-flatten xs)
  (if (null? xs)
      '()
      (if (list? xs)
          (append (my-flatten  (car xs)) (my-flatten (cdr xs)))
          (list xs))))
      
(my-flatten '((1) 2 (3 (4 5)) 6))

      






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

  (define (loop f start  xs)
    (if (>= (length xs) 2)
        (loop  f (car xs) (cons (f (car xs) (cadr xs)) (cddr xs) ) )
        (car xs) ))
  (define (my-fold-left f xs)
    (if (> (length xs) 1)
        (loop f (car xs)  xs)
        (car xs)))
  (display "#5\n" )
  (my-fold-left  quotient '(16 2 2 2 2))
  (my-fold-left  quotient '(1))
  (my-fold-left expt     '(2 3 4))
  ;
  ;
  ;
  (define (loop2 f start xs)
    (if (>= (length xs) 2)
        (loop2  f (car xs) (cons (f (cadr xs) (car xs)) (cddr xs) ) )
        (car xs) ))
  (define (my-fold-right f xs)
    (if (> (length xs) 1)
        (loop2 f (car (reverse xs))  (reverse xs))
        (car xs)))
  (display "#6\n" )
  (my-fold-right expt     '(2 3 4))  
  (my-fold-right expt     '(2)) 
  