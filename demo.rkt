(define out (open-output-file "data"))
 (display "hello" out)
 (close-output-port out)
 (define in (open-input-file "data"))
 (read in)

(close-input-port in)