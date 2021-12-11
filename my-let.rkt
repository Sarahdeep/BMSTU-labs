
(define-syntax my-let
  (syntax-rules ()
    
     ((my-let ((def expr)) . actions)
        ((lambda (def)  (begin . actions)) expr))
     
     ((my-let ((def expr) . xs) . actions)
      (my-let  xs ((lambda (def) (begin . actions)) expr)))))
      
(define-syntax my-let*
  (syntax-rules ()
    ((my-let* ((def expr)) . actions)
      ((lambda (def)  (begin . actions)) expr))
    ((my-let* ((def expr) . xs) . actions)
    ((lambda (def) (my-let* xs  . actions)) expr))))
        
       