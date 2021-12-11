(define-syntax trace-ex
  (syntax-rules ()
   ((trace-ex xs)
             (if (pair? xs)
                 `(xxs => ,xs)
                 `( car xxs => ,xs)))))