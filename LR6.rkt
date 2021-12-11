(define (check-frac str)
  (if (or (char-numeric? (string-ref  str 0)) (and (or (equal? (string-ref str 0) #\- ) (equal? (string-ref  str 0) #\+ ))  (char-numeric? (string-ref  str 1))))
      (lekser (string->list (substring   str 1)))
      #f))

(define (lekser xs)
  (cond
    ((char-numeric? (car xs)) (lekser (cdr xs)))
    ((and (equal? (car xs) #\/) (char-numeric? (cadr xs))) (denominate (cdr xs)))
    (else #f)))
(define  (denominate xs)
  (cond
    ((null? xs) #t)
    ((char-numeric? (car xs)) (denominate (cdr xs)))
    (else #f)))

(define (scan-frac str)
  (and (check-frac str) (let loop ((str (string->list str))
                                   (res '()))
                          (if (equal? (car str) #\/)
                              (eval (list '/ (string->number (list->string (reverse res)))
                                          (string->number (list->string (cdr str)))) (interaction-environment)) 
                              (loop (cdr str) (cons  (car str) res))))))  
                          
(define (scan-many-fracs str)
(let loop ((str (string-append str " "))
           (counter 0)
           (res '()))
  (cond
    ((equal? str "") (reverse res))
     ((and (char-whitespace? (string-ref str counter)) (not (= 0 counter)))
      (if (scan-frac (substring str 0 counter))
          (loop (substring str counter) 0 (cons (scan-frac (substring str 0 counter)) res))
          #f))
     ((char-whitespace? (string-ref str 0)) (loop (substring str 1) 0 res))
     ((char-numeric? (string-ref str counter)) (loop str (+ counter 1) res))
     (else (loop str (+ counter 1) res)))))





; =====================
; Синтаксический анализ
; =====================


(define x 4)
;<Program>  ::= <Articles> <Body> .
;<Articles> ::= <Article> <Articles> | .
;<Article>  ::= define word <Body> end .
;<Body>     ::= if <Body> endif <Body> | integer <Body> | word <Body> | .                     
(define (count program counter word res)
  (cond
    ((> (+ counter 1) (vector-length program))
     (if  res 
         res
         (x #f)))
    ((equal? (vector-ref program counter) word) (count program (+ counter 1) word counter))
     (else (count program (+ counter 1) word res))))

(define (countend program counter word)
  (cond
    ((> (+ counter 1) (vector-length program))
     #f)
     ((equal? (vector-ref program counter) word) counter)
     (else (countend program (+ counter 1) word))))

(define (parse tokens)   
     (call-with-current-continuation
 (lambda (error)
   (set! x error)
   (program  tokens 0)
   )))


 (define (program tokens counter)
   (let ((term (vector-ref tokens counter)))
   (cond
     ((equal? term 'define) (cons (articles tokens (+ counter 1)) (list (body tokens (+ (count tokens counter 'end #f) 1) 0))))
     (else (cons '() (list (body  tokens  counter  0)))))))
 
 (define (articles tokens counter)
   (let ((end (countend tokens counter 'define))) 
   (cons (article tokens counter) (if end (articles tokens (+ end 1) )
                                                  '()))))
 (define (article tokens counter )
   (let ((term (vector-ref tokens counter)))
     (list term (body tokens (+ counter 1) 1))))
 
 (define (body tokens counter flag)
   (if (> (+ counter 1) (vector-length tokens)) '()
      (let ((term (vector-ref tokens counter)))
   (cond
     ((equal? term 'if) (cons (list term (body  tokens (+ counter 1) flag))  (body tokens  (vipcounter tokens counter 0) flag )))
     ((equal? term 'end)
      (if (= flag 1)'()
          (x #f)))
     ((equal? term 'endif) '())
     (else (cons term (body  tokens (+ counter 1) flag )))))))
                        
   
   
  (define (vipcounter tokens counter  res)
    (if (> (+ counter 1) (vector-length tokens))
        (x #f)
 (let ((term (vector-ref tokens counter)))
   (cond
     ((equal? term 'if) (vipcounter tokens (+ counter 1)  (+ res 1)))
     ((equal? term 'endif) (vspom tokens res  counter )) 
     (else (vipcounter tokens (+ counter 1)  res))))))
  
  (define (vspom tokens res counter)
    (if (> (+ counter 1) (vector-length tokens))
        (x #f)
    (let ((term (vector-ref tokens counter)))
    (cond
      ((equal? term 'endif)(if (= res 1)
                               (+ counter 1)
                               (vspom tokens (- res 1) (+ counter 1))))
      ((and (equal? term 'if) (not (= res 0))) (x #f))
      (else (vspom tokens res (+ counter 1)))))))
      
   
 (display "TEST 1") (newline)
(parse #(1 2 +))
(newline)

(display "TEST 2") (newline)
(parse #(x dup 0 swap if drop -1 endif))
(newline)

(display "TEST 3") (newline)
(parse #(define abs
dup 0 <
if neg endif
end
9 abs
-9 abs))
(newline)

(display "TEST 4") (newline)
(parse #( define — 1 - end
define =0? dup 0 = end
define =1? dup 1 = end
define factorial
=0? if drop 1 exit endif
=1? if drop 1 exit endif
dup —
factorial
*
end
0 factorial
1 factorial
2 factorial
3 factorial
4 factorial ))
(newline)

(display "TEST 5") (newline)
(parse #(define =0? dup 0 = end
define <0? dup 0 < end
define signum
=0? if exit endif
<0? if drop -1 exit endif
drop
1
end
0 signum
-2 signum ))
(newline)

(display "TEST 6") (newline)
(parse #(define word w1 w2 w3))
(newline)

(display "TEST 7") (newline)
(parse #(1 2 if + if dup - endif endif dup))
(newline)

(display "TEST 8") (newline)
(parse #( define =0? dup 0 = end
define =1? dup 1 = end
define — 1 - end
define fib
=0? if drop 0 exit endif
=1? if drop 1 exit endif
— dup
— fib
swap fib
+
end
define make-fib
dup 0 < if drop exit endif
dup fib
swap —
make-fib
end
10 make-fib ))

      
      
      
    

                  
 ;(   ((-- (1 -))
 ;  (=0? (dup 0 =))
  ; (=1? (dup 1 =))
   ;(factorial
   ; (=0? (if (drop 1 exit)) =1? (if (drop 1 exit)) dup -- factorial *)))
  ;(0 factorial 1 factorial 2 factorial 3 factorial 4 factorial)  )       
    
  