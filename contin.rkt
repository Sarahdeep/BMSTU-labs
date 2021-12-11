(define call/cc call-with-current-continuation)
(define ie (interaction-environment))

(define r #f)
(define-syntax use-assertions
  (syntax-rules ()
    ((use-assertions) 
     (call/cc
      (lambda(cc)
        (set! r cc)
        )))))
(define-syntax assert
  (syntax-rules ()
    ((assert expr)
     (if (eval expr (interaction-environment))
         #t
         (begin
           (display "FAILED:  ")
           (r (display (quote expr)))
           )))))


(use-assertions)

(define (1/x x)
  (assert (not (zero? x)))
  (/ 1 x))
(map 1/x '(1 2 3 4 5))
(map 1/x '(-2 -1 0 1 2))