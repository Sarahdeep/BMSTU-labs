(define (save-data data path) 
  (with-output-to-file path #:exists 'truncate
    (lambda ()
      (write data))))
(define (load-data path)
  (with-input-from-file path
    (lambda()
      (read )
      )))
                         
(define x (load-data  "D:/tests.txt"))
(define y '(1 2 3 4))

(define (loop str port)
  (let ((e (read-char port))) 
    (if (not (eof-object? e))
        (loop (cons e str) port)
        str)))
(define (deleter str)
  (define (loop str res)
    (if (null? str)
        res
        (if (equal? (car str) #\return)
            (loop (cdr str) res)
            (loop (cdr str) (cons (car str) res)))))(loop str '()))
            
    
(define (filetolist path)
  (loop '() (open-input-file path)))

(define (line-counter path)
  (let ((xs   (string-append "\n" (list->string (deleter (filetolist path))) "\n" )))
    ;(write (filetolist path))
    (define (loop str count flag)
      (cond
        ((equal? str "") count)
        ((and (not (equal? (string-ref str 0)  #\newline))  (= flag 1))
         (loop (substring str 1) (+ count 1) 0))
        ((not (equal? (string-ref str 0)  #\newline))
         (loop (substring str 1)  count flag))
        (else (loop (substring str 1) count 1))))
    (loop (substring xs 1) 0 1)))

    
 
