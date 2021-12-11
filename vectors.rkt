(define (make-multi-vector . xs)
  (let ( (sizes (car xs))
         (fill
          (if (null? (cdr xs))
              0
              (cadr xs))))
    (  define  (loop sizes)
        (if (null? sizes)
            1
            (* (+ (car sizes) 1) (loop (cdr sizes)))))
    (make-vector (+ (loop sizes) 1) fill)))
(define m (make-multi-vector '(11 12 9 16)))
(vector-length m)
;_________________________________________________

(define (list->num xs)
  (define (loop xs res )
    (if  (= (length xs) 1)
         (+ res (car xs))
   (loop  (cdr xs) (* (+ res (car xs)) 10))))
  (loop xs 0))  



(define (multi-vector-set! vector xs def)
  (vector-set! vector (list->num xs) def))

(define (multi-vector-ref vector xs )
  (vector-ref vector (list->num xs) ))
;______________________________________________________

(multi-vector-set! m '(10 7 6 12) 'test)
(multi-vector-ref m '(10 7 7 2))
(multi-vector-set! m '(1 2 1 1) 'X)
(multi-vector-set! m '(2 1 1 1) 'Y)
(multi-vector-ref m '(1 2 1 1)) 
(multi-vector-ref m '(2 1 1 1))
(define m (make-multi-vector '(3 5 7) -1))
(multi-vector-ref m '(0 0 0))




  