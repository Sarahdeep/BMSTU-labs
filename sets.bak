(define (member? x xs) (and
                        (not (null? xs))
                        (or (equal? (car xs) x)
                            (member? x (cdr xs)))))
(define (list->set xs) (cond
                         ((null? xs) '())
                         ((member? (car xs) (cdr xs)) (list->set (cdr xs)))
                         (else (cons (car xs) (list->set (cdr xs))))))
(display "#1\n")
(list->set '(1 1  2 3))

(define (set? xs) 
  ( not
    (and
     (not (null? xs))
     (or
      (member? (car xs) (cdr xs))
      (not
       (set? (cdr xs)))))))
(display "#2\n")
(set? '( 2 1))




(define (union xs ys) (list->set (append xs ys)))
(display "#3\n")
(union '(1 2 3 ) '(2 3 4 5))





(define (intersection xs ys) (cond
                               ((or (null? xs) (null? ys))
                                '())
                               ((member? (car xs) ys)
                                (cons (car xs) (intersection (cdr xs) ys)))
                               (else
                                (intersection (cdr xs) ys))))
(display "#4\n")
(intersection '(1 2 3) '(2 3 4))



(define (difference xs ys) (cond
                             ((null? xs)
                              '())
                             ((member? (car xs) ys)
                              (difference (cdr xs) ys))
                             (else
                              (cons (car xs) (difference (cdr xs) ys)))))
(display "#5\n")
(difference '(1 2 3 4 5) '(2 3))




(define (symmetric-difference xs ys)(define (sym-dif zs) (cond
                                       ((null? zs)
                                        '())
                                       ((member? (car zs) (intersection xs ys)) 
                                        (sym-dif (cdr zs)))
                                       (else
                                        (cons (car zs) (sym-dif (cdr zs))))))
  (sym-dif (union xs ys)))
(display "#6\n")
(symmetric-difference '(1 2 3 4) '(3 4 5 6))







(display "#7\n")
(define (set-eq? xs ys) (= (length (append xs ys)) (* 2 (length (union xs ys)))))
(set-eq? '(1 2 3) '(3 2 1))                 
(set-eq? '(1 2) '(1 3))   

(define (set-eq?? xs ys) (equal? (symmetric-difference xs ys) '()))
(set-eq?? '(1 2 3) '(3 2 1))                 
(set-eq?? '(1 2) '(1 3))  
