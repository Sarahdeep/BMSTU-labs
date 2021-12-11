(define-syntax mderivative 
  (syntax-rules ()
    ((mderivative xs)
     (eval (derivative (quote xs))
           (interaction-environment)))))
