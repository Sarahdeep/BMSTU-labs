(load "support defenitions lab3.rkt")
(define (ref . xs)
  (if (twoargs? xs)
      (ref-type (car xs) (cadr xs))
      (insert-type (car xs) (cadr xs) (caddr xs))))
(ref '(1 2 3) 1 0)  ; ⇒ (1 0 2 3)
(ref #(1 2 3) 1 0)  ; ⇒ #(1 0 2 3)
(ref #(1 2 3) 1 #\0); ⇒ #(1 #\0 2 3)
(ref "123" 1 #\0)    ;⇒ "1023"
(ref "123" 1 0)    ;  ⇒ #f
(ref "123" 3 #\4)    ; "1234"
(ref "123" 5 #\4)    ; #f