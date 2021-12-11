(define (trib n)
  (cond
    ((<= n 1) 0)
    ((= n 2) 1)
    ((> n 2) (+ (trib (- n 1)) (trib (- n 2)) (trib (- n 3)))))) 
(define memo-trib
  (let ((known-results '((0 0) (1 0) (2 1))))
    (lambda (n)
      (let* ((args  n)
             (res (assoc args known-results)))
        (if res
            (cadr res)
            (let ((res (+ (memo-trib (- n 1)) (memo-trib (- n 2)) (memo-trib (- n 3)))))
              (set! known-results (cons (list args res) known-results))
              res))))))
     