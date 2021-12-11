(define-syntax test
  (syntax-rules ()
    ( (test def  res )
      (begin 
      
      (let ((xs (quote def))
            (result res))
        (list xs result))))))
      

