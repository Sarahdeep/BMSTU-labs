(define (ref-vector xv pos)
  (if (> (+ 1 pos) (vector-length xv))
      #f
      (vector-ref xv pos)))
(define (ref-string str pos)
  (if (> (+ 1 pos) (string-length str))
      #f
      (string-ref str pos)))

(define (ref-type xss pos)
  (cond ((vector? xss) (ref-vector xss pos))
        ((string? xss) (ref-string xss pos))
        ((list? xss) (ref-vector (list->vector xss) pos))
        (else #f)))

(define (twoargs? xss)
  (cond ((null? (cddr xss)) #t)
        (else #f)))

(define (loop xs pos num res )
  (if (= pos 0)
      (append (reverse (cons num res))  xs)
      (loop (cdr xs) (- pos 1) num (cons (car xs) res))))

(define (insert-vector xv pos num)
  (and (<= pos (vector-length xv)) 
       (list->vector (loop (vector->list xv) pos num '()))))
     
(define (insert-list xs pos num)
  (and (<= pos (length xs)) 
       (loop xs pos num '())))

(define (insert-str str pos num)
  (and (<= pos (string-length str)) (and (char? num) (list->string (loop (string->list str) pos num '())))))
       

(define (insert-type xss pos num)
  (cond ((vector? xss) (insert-vector xss pos num))
        ((string? xss) (insert-str xss pos num))
        ((list? xss) (insert-list xss pos num))
        (else #f)))







