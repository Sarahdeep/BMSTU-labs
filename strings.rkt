

(define counter 0)
(define (string-trim-left str) 
  (if (char-whitespace? (string-ref str counter))
      (begin (set! counter (+ counter 1))
             (string-trim-left str))
      (let((local counter))
        (set! counter 0)
        (substring str local))
      ))
(string-trim-left  "\t\tabc def")
(display #\newline)
;________________________________________________________________________________________________;
(define (string-trim-right str)
  (define counter-ach (- (string-length str) 1))
  (define (loop str)  
    (if (char-whitespace? (string-ref str counter-ach))
        (begin (set! counter-ach (- counter-ach 1))
               (loop str))
        (substring str 0 (+ counter-ach 1))))
  (loop str))
(string-trim-right "abc def\t") 
(display #\newline)   
;_________________________________________________________________________________________________________________;
(define (string-trim str) (string-trim-left (string-trim-right str)))
(string-trim       "\t abc def \n")
(display #\newline)
;________________________________________________________
(define (string-prefix? a b)
  (and
   (>= (string-length b) (string-length a))
   (equal? a (substring b 0  (string-length a))) ))
(string-prefix? "abc" "abcdef")   
(string-prefix? "bcd" "abcdef")  
(string-prefix? "abcdef" "abc")
(display #\newline)
;__________________________________________________________________________________________________________
(define (string-suffix? a b)
  (and
   (>= (string-length b) (string-length a))
   (equal? a (substring b (- (string-length b) (string-length a)) )) ))
(string-suffix? "def" "abcdef") 
(string-suffix? "bcd" "abcdef")
(display #\newline)
;__________________________________________________________________________________________________________

(define (string-infix? a b)
  (and
   (>= (string-length b) (string-length a))
   (or
    (equal? a (substring b 0  (string-length a)))
    (string-infix? a (substring b 1 )))))
    




(string-infix? "def" "abcdefgh")
(string-infix? "abc" "abcdefgh")
(string-infix? "fgh" "abcdefgh") 
(string-infix? "ijk" "abcdefgh")
(string-infix? "bcd" "abc")      
(display #\newline)
;__________________________________________________________________________________________________________

(define (string-split str sep)
  (define (loop str sep counter res) 
    (if (= 0 (string-length str))
        res
        (if (and
             (>= (string-length str) (string-length sep) )
             (equal? sep (substring str 0  (string-length sep)))
             (not (= counter 0))) 
              (loop (substring str (string-length sep)) sep 0 (append res (string (string-ref str counter)) ))

          
            (loop (substring str 1) sep (+ counter 1 ) res))))
  (loop str sep 0 '()))
  
 


  (string-split "x;y;z" ";")      
  (string-split "x-->y-->z" "-->")
  (display #\newline)
  ;__________________________________________________________________________________________________________
  ;Multivector























  



  