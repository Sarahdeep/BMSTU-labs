(define feature-if-else #t)
(define feature-while-loop #t)
(define feature-nested-if #t)
(define feature-break-continue #t)
(define feature-repeat-until #t)
(define feature-for-loop #t)
(define is_true #f)
(define ie (interaction-environment))
(define (interpret program stack) 
  (define (main counter stack return-stack dictionary)
    (if (=  counter (vector-length program) )
        stack
        (let ((word (vector-ref  program counter)))
          (cond ((number? word) (main (+ counter 1) (cons  word stack) return-stack dictionary))
                ((or (equal? word '+)  (equal? word '*) (equal? word '-) (equal? word '/))
                 (main (+ counter 1) (cons ((eval word ie) (cadr stack) (car stack)) (cddr stack)) return-stack dictionary))
                ((equal? word 'mod) (main (+ counter 1) (cons (remainder (cadr stack) (car stack)) (cddr stack)) return-stack dictionary))
                ((equal? word 'neg) (main (+ counter 1) (cons (- 0 (car stack)) (cdr stack)) return-stack dictionary))
                ((or (equal? word '=) (equal? word '>) (equal? word '<))
                 (main (+ counter 1) (cons (if ((eval word ie) (cadr stack) (car stack)) -1 0) (cddr stack)) return-stack dictionary))
                ((equal? word 'not) (main (+ counter 1) (cons (if  (= (car stack) 0) -1 0) (cdr stack)) return-stack dictionary))
                ((equal? word 'and) (main (+ counter 1) (cons (if (not (or (equal? (car stack) 0)
                    (equal? (cadr stack) 0))) -1 0) (cddr stack)) return-stack dictionary))
                ((equal? word 'or) (main (+ counter 1) (cons (if (or (not (equal? (car stack) 0))
                    (not (equal? (cadr stack) 0))) -1 0) (cddr stack)) return-stack dictionary))
                ((equal? word 'drop) (main (+ counter 1)  (cdr stack) return-stack dictionary))
                ((equal? word 'swap)
                 (main (+ counter 1) (append  (list (cadr stack)) (list (car stack)) (cddr stack)) return-stack dictionary))
                ((equal? word 'dup) (main (+ counter 1) (cons (car stack)  stack) return-stack dictionary))
                ((equal? word 'over) (main (+ counter 1) (cons  (cadr stack)   stack) return-stack dictionary))
                ((equal? word 'rot)
                 (main (+ counter 1) (append (list (caddr stack) (cadr stack) (car stack))   (cdddr stack)) return-stack dictionary))
                ((equal? word 'depth) (main (+ counter 1) (cons (length stack)  stack) return-stack dictionary))
                ((equal? word 'define) (main (count counter program 'end) stack  return-stack (cons (list  (vector-ref  program (+ counter 1)) (+ counter 2)) dictionary)))
                ((assoc word dictionary) (main (cadr (assoc word dictionary)) stack (cons (+ counter 1) return-stack) dictionary))
                ((equal? word 'end) (main (car return-stack) stack (cdr return-stack) dictionary))
                ((equal? word 'exit) (main (car return-stack) stack (cdr return-stack) dictionary))
                ((equal? word 'if) (let ((index (if-counter program (+ counter 1) 0 #f)))
                                     (if (= (car stack) 0)               
                   (main (+ index 1) (cdr stack)  return-stack  dictionary)
                                         (if (equal? (vector-ref program index) 'endif)
                      (main (+ counter 1) (cdr stack)  return-stack  dictionary)                       
                   (main (+ counter 1) (cdr stack)  (cons (if-counter program (+ counter 1) 0 #t) return-stack) dictionary) ))))
                ((equal? word 'else)
                 (main  (car return-stack)  stack  (cdr return-stack) dictionary))    
                ((equal? word 'endif) (main (+ counter 1) stack   return-stack dictionary))
                ((equal?  word 'while) (if (= (car stack) 0)
                 (main (+ (w-counter program (+ counter 1) 0) 1) (cdr stack) return-stack dictionary)
                 (main (+ counter 1) (cdr stack) (cons  counter return-stack) dictionary)))
                ((equal? word 'wend) (main (car return-stack) stack (cdr return-stack) dictionary))
                ((equal? word 'repeat) (main  (+ counter 1) stack (cons counter return-stack) dictionary))  
                ((equal? word 'until) (if (= (car stack) 0)                         
                 (main (car return-stack) (cdr stack) (cdr return-stack) dictionary))
                                     (main (+ counter 1)  (cdr stack) (cdr return-stack) dictionary))
                ((equal? word 'break) (main (+ (breaker counter program) 1) stack (cdr return-stack) dictionary))  
                ((equal? word 'continue) (main (breaker counter program)  stack return-stack dictionary))
                  ((equal? word 'for ) (main (+ counter 1) (cddr stack) (cons (cadr stack) (cons (car stack) (cons (+ counter 1) return-stack))) dictionary))
                ((equal? word 'i) ;(> (or (count counter program 'for) (+ (count counter program 'next) 1)) (count counter program 'next)))
                 (main (+ counter 1) (cons (car return-stack) stack) return-stack dictionary))
                ((equal? word 'next) (if (> (+ (car return-stack) 1) (cadr return-stack))
                                         (main (+ counter 1) stack (cdddr return-stack) dictionary)
                                        ( main (caddr return-stack) stack (cons (+ (car return-stack) 1) (cdr return-stack)) dictionary))) 
                ))))
    (main 0 stack '() '()))
        (define (breaker counter vect)
          (let ((word (vector-ref  vect counter)))
            (if (or (equal? word 'wend) (equal? word 'until) (equal? word 'next))
                counter
            (breaker (+ counter 1) vect))
            ))
        
(define (count counter vect word)
  (cond ((> counter  (- (vector-length vect) 1)) #f)  
        ((equal? (vector-ref  vect counter) word) (+ counter 1)) 
        (else (count (+ counter 1) vect word))))
(define (countif counter vect stopword word)
  (cond ((> counter  (- (vector-length vect) 1)) #f)
        ((equal? (vector-ref vect counter) word) (+ (countif (+ counter 1) vect stopword word) 1))  
        ((equal? (vector-ref  vect counter) stopword) (+ counter 1)) 
        (else (countif (+ counter 1) vect stopword word))))

(define (if-counter program index count is_true)
(let ((word (vector-ref program index)))
(cond ((and (= 0 count) (equal? word 'endif)) index )
      ((and (= 0 count) (not is_true) (equal? word 'else))  index)
       ((equal? word 'if) (if-counter program (+ index 1) (+ count 1) is_true))
      ((equal? word 'endif) (if-counter program (+ index 1) (- count 1) is_true))
      (else (if-counter program (+ index 1) count is_true) ))))
  (define (w-counter program index count)
(let ((word (vector-ref program index)))
(cond ((> index (vector-length program)) #f)
  ((and (= 0 count) (equal? word 'wend)) index )
       ((equal? word 'while) (w-counter program (+ index 1) (+ count 1)))
      ((equal? word 'wend) (w-counter program (+ index 1) (- count 1)))
      (else (w-counter program (+ index 1) count) ))))
                                     
                                     
  (interpret #(define =0? dup 0 = end repeat dup 2 mod swap 2 / =0? until drop) '(1))

    