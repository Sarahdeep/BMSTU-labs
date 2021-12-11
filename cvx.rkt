(define (string-split str sep)
  (define (loop str sep counter res) 
    (if (= 0 (string-length str))
        res
        (if (and
             (>= (string-length str) (string-length sep) )
             (equal? sep (substring str counter  (+(string-length sep) counter)))
             (not (= counter 0))) 
              (loop (substring str (+ (string-length sep) counter)) sep 0 (append res (list  (substring str 0 counter ) )))

          
            (loop str sep (+ counter 1 ) res))))
  (loop (string-append str sep) sep 0 '()))
  
 


  (string-split "fx;gy;zf;" ";")      
  (display #\newline)