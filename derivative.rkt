(define (lister xs)
  (if (list? xs)
  xs
  (list xs)))

(define (derivative xs)
  (cond ((number? (car xs)) 0)
        
        ((equal? (car xs) 'x) 1)
        ((equal? (car xs) '-x) -1)
        ((equal? (car xs) '-)
         (if (> (length xs) 3)
             `(- ,(derivative  (lister (cadr xs))) ,(derivative (cons '+ (cddr xs))))    
             `(- ,(derivative  (lister (cadr xs))) ,(derivative (lister (caddr xs))))))
        ((equal? (car xs) '*)
         (if (> (length xs) 3)
             `(+ (* ,(derivative (lister (cadr xs))) ,(cons '* (cddr xs))) (* ,(cadr xs) ,(derivative (cons '* (cddr xs)))))   
             `(+ (* ,(derivative (lister (cadr xs))) ,(caddr xs)) (* ,(cadr xs) ,(derivative (lister (caddr xs)))))))
        ((equal? (car xs) 'expt)
         (if (member 'x (lister (caddr xs)))
             `(* (expt ,(cadr xs) ,(caddr xs)) (log ,(cadr xs) ,(exp 1)) ,(derivative (lister (caddr xs))))   
             (if (equal? (cadr xs) 'x)
                 `(* ,(caddr xs) (expt  x ,(- (caddr xs) 1)))   
                 `(* ,(caddr xs) (expt  ,(derivative (lister (cdr xs))) ,(- (caddr xs) 1))))))
        ((equal? (car xs) 'exp)
         `(* (exp ,(cadr xs)) ,(derivative (lister (cadr xs)))))
        ((equal? (car xs) 'log)
         `(* (/ 1 ,(cadr xs)) ,(derivative (lister (cadr xs)))))
        ((equal? (car xs) '/)
         (if (> (length xs) 3)
             `(/ (- (* ,(derivative (lister (cadr xs))) ,(cons '* (cddr xs))) (* ,(cadr xs) ,(derivative (cons '* (cddr xs))))) (expt ,(cons '* (cddr xs)) 2)) 
             `(/ (- (* ,(derivative (lister (cadr xs))) ,(caddr xs)) (* ,(cadr xs) ,(derivative (lister (caddr xs))))) (expt ,(caddr xs) 2))))
        ((equal? (car xs) 'cos)
         `(* -1 (sin ,(cadr xs)) ,(derivative (lister (cadr xs))))) 
           ((equal? (car xs) 'sin)
         `(*  (cos ,(cadr xs)) ,(derivative (lister (cadr xs)))))
        ((equal? (car xs) '+)
         (if (> (length xs) 3)
             `(+ ,(derivative (lister (cadr xs))) ,(derivative (cons '+ (cddr xs))))
             `(+ ,(derivative (lister (cadr xs))) ,(derivative (lister (caddr xs))))))))


(define ie (interaction-environment))
(define x 1)
(define the-tests
 (list (test (derivative '(2)) '0) ;1
        (test (eval (derivative '(x)) ie) '1) ;2
        (test (eval (derivative '(-x)) ie) '-1) ;3
        (test (eval (derivative '(* 1 x)) ie) (eval '(+ (* 0 x) (* 1 1)) ie)) ;4
        (test (eval (derivative '(* -1 x)) ie) (eval '(+ (* 0 x) (* -1 1))ie)) ;5
        (test (eval (derivative '(* -4 x)) ie) (eval '(+ (* 0 x) (* -4 1)) ie)) ;6
        (test (eval (derivative '(* 10 x)) ie) (eval '(+ (* 0 x) (* 10 1)) ie)) ;7
        (test (eval (derivative '(- (* 2 x) 3)) ie) (eval '(- (+ (* 0 x) (* 2 1)) 0) ie)) ;8
        (test (eval (derivative '(expt x 10)) ie) (eval '(* 10 (expt x 9)) ie)) ;9 
        (test (eval (derivative '(* 2 (expt x 5))) ie) (eval '(+ (* 0 (expt x 5)) (* 2 (* 5 (expt x 4)))) ie)) ;10
        (test (eval (derivative '(expt x -2)) ie) (eval '(* -2 (expt x -3)) ie)) 
        (test (eval (derivative '(expt 5 x)) ie) (eval '(* (expt 5 x) (log 5 2.718281828459045) 1) ie)) 
        (test (eval (derivative '(cos x)) ie) (eval '(* -1 (sin x) 1) ie))
        (test (eval (derivative '(sin x)) ie) (eval '(* (cos x) 1) ie)) 
        (test (eval (derivative '(exp x)) ie) (eval '(* (exp x) 1) ie)) 
        (test (eval (derivative '(* 2 (exp x))) ie) (eval '(+ (* 0 (exp x)) (* 2 (* (exp x) 1))) ie)) 
        (test (eval (derivative '(* 2 (exp (* 2 x)))) ie) (eval '(+ (* 0 (exp (* 2 x))) (* 2 (* (exp (* 2 x)) (+ (* 0 x) (* 2 1))))) ie)) 
        (test (eval (derivative '(log x)) ie) (eval '(* (/ 1 x) 1) ie)) 
        (test (eval (derivative '(* 3 (log x))) ie) (eval '(+ (* 0 (log x)) (* 3 (* (/ 1 x) 1))) ie)) 
        (test (eval (derivative '(+ (expt x 3) (expt x 2))) ie)
              (eval '(+ (* 3 (expt x 2)) (* 2 (expt x 1))) ie)) 
        (test (eval (derivative '(- (* 2 (expt x 3)) (* 2 (expt x 2)))) ie)
              (eval '(- (+ (* 0 (expt x 3)) (* 2 (* 3 (expt x 2)))) (+ (* 0 (expt x 2)) (* 2 (* 2 (expt x 1))))) ie))
        (test (eval (derivative '(/ 3 x)) ie)
              (eval '(/ (- (* 0 x) (* 3 1)) (expt x 2)) ie)) 
        (test (eval (derivative '(* 3/2 (expt x -2))) ie)
              (eval '(+ (* 0 (expt x -2)) (* 3/2 (* -2 (expt x -3)))) ie)) 
        (test (eval (derivative '(* 2 (sin x) (cos x))) ie)
              (eval '(+ (* 0 (* (sin x) (cos x))) (* 2 (+ (* (* (cos x) 1) (cos x)) (* (sin x) (* -1 (sin x) 1))))) ie)) 
        (test (eval (derivative '(* 2 (exp x) (sin x) (cos x))) ie)
             (eval  '(+ (* 0 (* (exp x) (sin x) (cos x))) (* 2 (+ (* (* (exp x) 1) (* (sin x) (cos x))) (* (exp x) (+ (* (* (cos x) 1) (cos x)) (* (sin x) (* -1 (sin x) 1))))))) ie)) 
        (test (eval (derivative '(sin (* 2 x))) ie)
             (eval '(* (cos (* 2 x)) (+ (* 0 x) (* 2 1))) ie))
        (test (eval (derivative '(cos (* 2 (expt x 2)))) ie)
             (eval '(* -1 (sin (* 2 (expt x 2))) (+ (* 0 (expt x 2)) (* 2 (* 2 (expt x 1))))) ie)) 
        (test (eval (derivative '(sin (log (expt x 2)))) ie)
             (eval  '(* (cos (log (expt x 2))) (* (/ 1 (expt x 2)) (* 2 (expt x 1)))) ie)) 
        (test (eval (derivative '(+ (sin (* 2 x)) (cos (* 2 (expt x 2))))) ie)
             (eval '(+ (* (cos (* 2 x)) (+ (* 0 x) (* 2 1))) (* -1 (sin (* 2 (expt x 2))) (+ (* 0 (expt x 2)) (* 2 (* 2 (expt x 1)))))) ie)) 
        (test (eval (derivative '(* (sin (* 2 x)) (cos (* 2 (expt x 2))))) ie)
             (eval '(+ (* (* (cos (* 2 x)) (+ (* 0 x) (* 2 1))) (cos (* 2 (expt x 2)))) (* (sin (* 2 x)) (* -1 (sin (* 2 (expt x 2))) (+ (* 0 (expt x 2)) (* 2 (* 2 (expt x 1))))))) ie))))
(run-tests the-tests)