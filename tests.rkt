(load "unit-test.scm")
(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0)  1) ;
    (else     1)))
(define (run-test xs)
  (display #\newline)
  (display  (car xs))
  (let
      ((head (eval (car xs)
                   (interaction-environment)))
       (expect (cadr xs)))
 
    (if (equal? head expect)
        (begin
          (display " ok")
          #t)
        (begin
          (display "  FAIL")
          (display #\newline)
          (display "Expected:")
          (write expect)
          (display #\newline)
          (display "Returned:")
          (write head)
          ;(display #\newline)
          #f))))







(define (run-tests xs)
  (define (loop xs count)
    (if (null? xs)
        (begin
          (display #\newline)
          count )
        (if (run-test (car xs))
            (loop (cdr xs) (if count count
                               #f))
            (loop (cdr xs) #f))))(loop xs #t))











