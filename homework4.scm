(define (memoized-factorial n)
  (let (
        (prev 1)
        (x 1))
    (define (loop n res)
      (if (= n 0)
          res
          (begin
            (set! prev res)
            (set! res (* prev x))
            (set! x (+ x 1))
            (loop (- n 1)  res))))(loop n 1)))

(define-syntax lazy-cons
  (syntax-rules()
    ((lazy-cons a b)
     (cons a (delay b)))))
  
(define (lazy-car p)
  (car p))
(define (lazy-cdr p)
  (force (cdr p)))
  
(define (lazy-head xs k)
  (if (> k 0)
      (cons (lazy-car xs) (lazy-head (lazy-cdr xs) (- k 1)))
      '()))
(define (lazy-ref xs k)
  (define (loop xs k)
    (if (< k 2)
        (lazy-car xs)
        (loop (lazy-cdr xs) (- k 1) )))(loop xs (+ k 1)))
    
(define (naturals start)
  (lazy-cons start (naturals (+ start 1))))
  
    
(define (lazy-factorial n)
  (let (
        ( prev 1)
        (x 1)
        (res 1)
        )
    (define (fact n)
      (begin
        (set! prev res)
        (set! res (* x prev))
        (set! x (+ x 1))
        (lazy-cons res (fact res))))
    (lazy-ref (fact n) n)))

;(define (loop word  list port flag)
; (let ((e (read-char port)))
;  (if (not (eof-object? e))
;     (if (not (char-whitespace? e))
;       (loop (cons e word) list port)
;      (loop '() (append list (list->string (reverse word))) port)
;       (loop (cons e str) port))
;       str)))

(define (loop str)
  (let ((e (read-char))) 
    (if (not (eof-object? e))
        (loop (cons e str))
        str)))

(define (filetolist)
  (loop '() ))


(define (read-words)
  (let ((xs    (string-append "\n" (list->string (reverse (filetolist))) "\n" )))
    (display xs)
    (define (loop xs counter list flag)
      (cond
        ((equal? xs "")  (reverse list))
        ((and (char-whitespace? (string-ref xs counter)) (= flag 1))
         (loop (substring xs counter) 0 (cons  (substring xs 0 counter) list ) 0))
        ((char-whitespace? (string-ref xs counter))
         (loop (substring xs 1) 0 list flag))
        (else
         (loop xs (+ counter 1) list 1))))(loop xs 0 '() 0)))
   
;____________________________________________________













;______________________________
(define ie (interaction-environment))
(define-syntax define-struct
  (syntax-rules ()
    ((define-struct type (names ...))
     (begin
       (eval (list 'define  (string->symbol (string-append (symbol->string 'make-) (symbol->string 'type)))
                   '(lambda (names ... )
                      (list 'type  (list (list 'names names ) ...)))) ie)
       (eval  (list 'define  (string->symbol (string-append (symbol->string 'type) (symbol->string '?)))
                    '(lambda (xs) 
                       (and (list? xs) (not (null? xs)) (equal? (car xs) 'type) ))) ie)
    
       (eval  (list 'define  (string->symbol (string-append (symbol->string 'type)(symbol->string '-) (symbol->string 'names)))
                    '(lambda (xs)
                       (cadr (assoc 'names  (cadr xs))))) ie) ...

                                                              (eval  (list 'define  (string->symbol (string-append (symbol->string 'set-) (symbol->string 'type)(symbol->string '-) (symbol->string 'names) (symbol->string '!)))
                                                                           '(lambda (xs num)
                                                                              (set-car! (cdr (assoc 'names  (cadr xs))) num))) ie) ...                                     
                                                                                                                                   ))))
     
(define-struct pos (row col))
(define p (make-pos 1 2)) 
;____________________________________________________________





;__________________
(define-syntax define-data
  (syntax-rules ()
    ((define-data type ((types .( params ...)) ...))
     (begin
       (eval (list  'define 'types 
                    (lambda (params ...) (list 'type (list  'types
                                                            ;(list 'params params)
                                                            (list params ...)
                                                            ))))  ie) ...

                                              (eval  (list 'define  (string->symbol (string-append (symbol->string 'type) (symbol->string '?)))
                                        '(lambda (xs)
                                    (and (list? xs) (not (null? xs)) (equal? (car xs) 'type) ))) ie)                                                                   
                                                                      ))))
      
(define (assmaker params defenitions)
  (if (null? params)
      '()
      (cons (list (car params)  (car defenitions)) (assmaker (cdr params)  (cdr defenitions)))))
(define-syntax match
  (syntax-rules ()
    
    ((match f ((sample . (param ...)) defenition) ...)


     (cond ((equal?  'sample  (caadr f))
            (let* ((listik (cadr (cadr f)))
                   (asslist (assmaker '(param ...) listik)))
              ((lambda   ( param ...)   defenition )  (cadr (assoc 'param asslist))...)))  ... ))))
(define-data figure ((square a)
                     (rectangle a b)
                     (triangle a b c)
                     (circle r)))
(define s (square 10))
(define r (rectangle 10 20))
(define t (triangle 10 20 30))
(define c (circle 10))


               
(define (perim f)
  (match f 
    ((square a)       (* 4 a))
    ((rectangle a b)  (* 2 (+ a b)))
    ((triangle a b c) (+ a b c))
    ((circle r)       (* 2 pi r))))



