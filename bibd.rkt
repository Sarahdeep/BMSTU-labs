
    
(define (make-multi-vector . xs)
  (let ( (sizes (car xs))
         (fill
          (if (null? (cdr xs))
              0
              (cadr xs))))
    (vector  sizes  fill)))


(define m (make-multi-vector '(11 12 9 16)))

(define (my-predicate vect num)
  (or (null? num)
      (and (>= (car  vect ) (car num))
           (my-predicate (cdr vect ) (cdr num)  ))))
  
(define (multi-vector-set! vect num def) (define count 0) (define res (vector-length vect ))
  (cond  ((and (my-predicate (vector-ref vect 0) num)  (= (loop count res num vect) 0))
          (set! m (list->vector (append (vector->list m) (list num) (list def)))))
         ((my-predicate (vector-ref vect 0) num)
          (begin (vector-set! vect (loop count res num vect)  0)
                 (vector-set! vect  (+ (loop count res num vect) 2)  0)
                 (set! m (list->vector (append (vector->list m) (list num) (list def)))))
          )
         (else #f)))


( define (loop count res num vect)
   (cond  ((equal? (vector-ref vect count) num) count)
          ((= count (- res 1))
           0)
          (else (loop (+ count 1) res num vect))))
          
   




(define (multi-vector-ref vect num ) (define count 0) (define res (vector-length vect ))
  (if (my-predicate (vector-ref vect 0) num)
      (vector-ref vect (+ (loop count res num vect) 1))
      #f))
  
(multi-vector-set! m '(1 2 1 1) 'X)
(multi-vector-set! m '(2 1 1 1) 'Y)
(multi-vector-ref m '(1 2 1 1)) 
(multi-vector-ref m '(2 1 1 1)) 














