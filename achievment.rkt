(define (list-trim-left xs)
  (define  (loop xs)
    (cond
      ((null? xs) '())
      ((not (char? (car xs))) xs)
      ((char-whitespace? (car xs))
       (loop (cdr xs)))))
  (loop xs))
  
(define (list-trim-right xs)
  (define (loop xs res)
    ( if (null? (list-trim-left xs))
         res
         (loop (cdr xs) (append res (list (car xs))))))(loop xs '()))