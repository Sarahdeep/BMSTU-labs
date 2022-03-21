;; Конструктор потока
(define (make-stream items . eos)
  (if (null? eos)
      (make-stream items #f)
      (list items (car eos))))

;; Запрос текущего символа
(define (peek stream)
  (if (null? (car stream))
      (cadr stream)
      (caar stream)))

;; Запрос первых двух символов
(define (peek2 stream)
  (if (null? (car stream))
      (cadr stream)
      (if (null? (cdr(car stream)))
          (cadr stream) 
          (next stream))))

;; Продвижение вперёд
(define (next stream)
  (let ((n (peek stream)))
    (if (not (null? (car stream)))
        (set-car! stream (cdr (car stream))))
    n))


; =====================
; Синтаксический анализ
; =====================

(define (tokenize str)
  (let* ((EOF (integer->char 0))
         (stream (make-stream (string->list str) EOF)))
    
    (call-with-current-continuation
     (lambda (error)
       (define result (tokens stream error))
       (and (equal? (peek stream) EOF)
            result)))))

(define (tokens stream error)
  (define (start-token? char)
    (or ( char-alphabetic? char)
        (char-numeric? char)
        (equal? char #\()
        (equal? char #\))
        (operator? char)))
  
  
  (cond ((char-whitespace? (peek stream))
         (spaces stream error)
         (tokens stream error))
        ((start-token? (peek stream))
         (cons (token stream error)
               (tokens stream error)))
        (else '())))
(define (operator? char)
  (or
   (equal? char #\+)
   (equal? char #\-)
   (equal? char #\*)
   (equal? char #\/)
   (equal? char #\^)))

(define (spaces stream error)
  (cond ((char-whitespace? (peek stream))
         (next stream))))

(define (token stream error)
  (cond ((equal? (peek stream) #\() (make-string 1 (next stream)))
        ((equal? (peek stream) #\)) (make-string 1 (next stream)))
        ((operator? (peek stream)) (string->symbol (make-string 1 (next stream))))
        
        ((char-alphabetic? (peek stream))
         (variable-or-keyword stream error))
        ((char-numeric? (peek stream))
         (numeric stream error))
        (else (error #f))))
  
(define (variable-or-keyword stream error)
  (cond ((char-alphabetic? (peek stream))
         (string->symbol
          (list->string (cons (next stream)
                              (variable-tail stream error)))))
        (else (error #f))))
  
(define (variable-tail stream error)
  (cond ((char-alphabetic? (peek stream))
         (cons (next stream)
               (variable-tail stream error)))
        (else '())))
  
(define (numeric stream error)
  (cond ((char-numeric? (peek stream))
         (string->number
          (list->string (cons (next stream)
                              (number-tail stream error)))))
        (else (error #f))))
  
  

    
(define (number-tail stream error)
  (cond ((char-numeric? (peek stream))
         (cons (next stream)
               (variable-tail stream error)))
        ((equal? (peek stream) #\.)
         (cons (next stream)
               (point-tail stream error)))
        ((and (equal? (peek stream) #\e)   (definer? stream))
         (cons (next stream)
               (e-tail stream error)))
        (else '())))


(define (definer? stream)
  (or (char-numeric? (peek2 stream))  (equal? (peek2 stream) #\-) (equal? (peek stream) #\+)))

(define (point-tail stream error)
  (cond ((char-numeric? (peek stream))
         (cons (next stream)
               (variable-tail stream error)))
        ((and (equal? (peek stream) #\e)   (definer? stream))
         (cons (next stream)
               (e-tail stream error)))
        (else '())))

(define (e-tail stream error)
  (first-exp stream error))
 

(define (first-exp stream error)
  (cond
    ((char-numeric? (peek stream))
     (cons (next stream)
           (end-tail stream error)))
    ((or (equal? (peek stream) #\-) (equal? (peek stream) #\+))
     (cons (next stream)
           (end-tail stream error)))
    (else '())))
(define (end-tail stream error)
  (cond ((char-numeric? (peek stream))
         (cons (next stream)
               (end-tail stream error)))
        (else '())))
  
    
; =====================
; Синтаксический анализ
; =====================
;Expr    ::= Term Expr' .
;Expr'   ::= AddOp Term Expr' | .
;Term    ::= Factor Term' .
;Term'   ::= MulOp Factor Term' | .
;Factor  ::= Power Factor' .
;Factor' ::= PowOp Power Factor' | .
;Power   ::= value | "(" Expr ")" | unaryMinus Power

(define (parse str)
  (let* ((EOF (integer->char 0))
         (stream (make-stream  str EOF)))
    (call-with-current-continuation
     (lambda (error)
       (define result (expression stream error))
       (and (equal? (peek stream) EOF)
            result)))))


(define (expression stream error)
  (cond ( (equal? (peek stream) #\nul) #f)
        
         ((start-term? (peek stream))
         (let* (( pow (term stream error))
                (tail (expr0 stream error  pow)))
           (if (null? tail)
               pow
               tail)))
        (else  #f)))

(define (start-term? token)
   (variable? token))

(define (variable? token)
  (and 
   (not (equal? token '+))
   (not (equal? token '/))
   (not (equal? token '*))
   (not (null? token))
   (not (equal? token '^))))
(define (term stream error)

  (let* (( pow (factor stream error))
         (tail (term0 stream error  pow)))
    (if (null? tail)
        pow
        tail)))

(define (factor stream error)
       
  (let (( pow (power stream error))
        (tail (factor0 stream error)))
    (if (null? tail)
        pow
        (cons pow
              tail))))

(define (power stream error)
  (cond
    ( (string? (peek stream)) (next stream)
             (let ((expr (expression stream error)))
               (if (equal? (next stream) ")")
                   expr
                   (error #f))))
  
    ( (equal? (peek stream) '-)
      (list (next stream) (power stream error)))
    (else (next stream))))

(define (factor0 stream error)
  (if (equal? (peek stream) '^)
      (list (next stream)  (factor stream error))
      '()))


(define (term0 stream error res )
  (if (or (equal? (peek stream) '*)
          (equal? (peek stream) '/))
      (term0 stream error (list res  (next stream)  (factor stream error)))
      res))

(define (expr0 stream error res)
  (let ((token (peek stream)))
  (if (or (equal? token '+)
          (equal? token '-))
     (begin
       (next stream)
     (if (equal? (peek stream) #\nul)
         (error #f)
      (expr0 stream error (list res  token  (term stream error)))))
      res))) 
      
  

      
      
      
  ; Ассоциативность левая
;
(parse (tokenize "a/b/c/d"))
  

; Ассоциативность правая
;
(parse (tokenize "a^b^c^d"))


; Порядок вычислений задан скобками
;
(parse (tokenize "a/(b/c)"))


; Порядок вычислений определен только
; приоритетом операций
;
(parse (tokenize "a + b/c^2 - d"))
(display "\n")

(define (tree->scheme parsed)
  (if (list? parsed)
      (if (> (length parsed) 2)
      (if (equal? (cadr parsed) '^)
          (list 'expt (tree->scheme(car parsed)) (tree->scheme(caddr parsed)))
      (list (cadr parsed) (tree->scheme(car parsed)) (tree->scheme(caddr parsed))))
      parsed)
      parsed))


 

  
































