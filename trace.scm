(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex xs)
     (begin 
       (display (quote xs))
       (  display      "=>")
       (let ((xv xs))
         ( display xv)
         ( display #\newline)
         xv)))))